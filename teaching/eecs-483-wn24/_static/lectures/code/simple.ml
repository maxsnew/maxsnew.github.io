(* simple.ml

   Interpeter for a Simple IMperative Programming LanguagE.
*)

(* 
 *
 * Recall the BNF grammar for this SIMPLE language:
 *
 *  <exp> ::= 
 *         |  <X>                       // variables
 *         |  <exp> + <exp>             // addition
 *         |  <exp> * <exp>             // multiplication
 *         |  <exp> < <exp>             // less-than
 *         |  <integer constant>        // literal
 *         |  (<exp>)
 *
 *  <cmd> ::= 
 *         |  skip
 *         |  <X> = <exp>
 *         |  ifNZ <exp> { <cmd> } else { <cmd> }
 *         |  whileNZ <exp> { <cmd> }
 *         |  <cmd>; <cmd>
 *
 *)


(* 
 *  OCaml datatypes that we use to represent SIMPLE abstract syntax.
 *  
 *  This is called _abstract syntax_ because it uses the labeled 
 *  tree structure rather than concrete keywords, punctuation marks, etc., 
 *  to represent the program.
 *
 *  For example, the concrete syntax for the following program:
 *                  (3 + X) * 2
 *  is the tree:
 *      Mul(Add(Lit 3, Var "X"), Lit 2)
 *)


type var = string

type exp =
  | Var of var
  | Add of exp * exp
  | Mul of exp * exp
  | Lt  of exp * exp
  | Lit of int

type cmd =
  | Skip
  | Assn of var * exp
  | IfNZ of exp * cmd * cmd
  | WhileNZ of exp * cmd
  | Seq of cmd * cmd


(* AST for the concrete syntax:
 *                  (3 + X) * 2
*)
let example = Mul(Add(Lit 3, Var "X"), Lit 2)



(* AST for the concrete syntax:

        X = 6;
	ANS = 1;
	whileNZ (X) {
  		ANS = ANS * X;
  		X = X + -1;
	} 
 *)
let factorial : cmd =
  let x = "X" in
  let ans = "ANS" in
  Seq(Assn(x, Lit 6),
      Seq(Assn(ans, Lit 1),
          WhileNZ(Var x,
                  Seq(Assn(ans, Mul(Var ans, Var x)),
                      Assn(x, Add(Var x, Lit(-1)))))))
    

(* interpreters and state --------------------------------------------------- *)

(* We can "interpret" a SIMPLE program by giving it a meaning in terms of 
 * OCaml operations.  One key question is how to represent the _state_ of
 * the SIMPLE program.  Out intuition tells us that it should be a map
 * that sends variables to their (current) values.  There are many ways that
 * we could represent such a state.  Here, we use OCaml's functions.
*)
type state = var -> int

(* The initial state maps every variable to 0 *)
let init_state : state = 
  fun x -> 0

(* We can update an old state [s] to one that maps `x` to `v` but is otherwise
 * unchanged by building a new function like this: *)
let update (s:state) (x:var) (v:int) : state =
  fun (y:var) ->
    if x = y then v else s y


(* Looking up the value of a variable in a state is easy: *)
let lookup (s:state) (x:var) : int = s x


(* To interpret an expression in a given state, we recursively compute the
 * values of subexpressions and then combine them according to the operation.
 *
 * One wrinkle: we have chosen to use only `int` as the domain of values,
 * so the result of a less-than comparison encodes "true" as 1 and "false" as 0.
 *)
let rec interpret_exp (s:state) (e:exp) : int =
  begin match e with
    | Var x -> lookup s x
    | Add(e1, e2) -> (interpret_exp s e1) + (interpret_exp s e2)
    | Mul(e1, e2) -> (interpret_exp s e1) * (interpret_exp s e2)
    | Lt(e1, e2) -> if (interpret_exp s e1) < (interpret_exp s e2) then 1 else 0
    | Lit i -> i
  end

(* To interpret a command, we write an OCaml program that manipulates that 
 * state as appropriate.  The result of running a command is a final state.
 * 
 * Note that `WhileNZ` "unfolds" the loop into a conditional that either 
 * runs the loop body once and the coninues as another `WhileNZ`, or just 
 * Skip.
 *
 * Note that the SIMPLE sequence of two commands is interpreted by the
 * sequencing of OCaml's `let` binding construct.
 *)
let rec interpret_cmd (s:state) (c:cmd) : state =
  begin match c with
    | Skip -> s
    | Assn(x, e) -> update s x (interpret_exp s e)
    | IfNZ(e, c1, c2) ->
      if (interpret_exp s e) <> 0 then interpret_cmd s c1 else interpret_cmd s c2
    | WhileNZ(e, c) ->
      interpret_cmd s (IfNZ(e, Seq(c, WhileNZ(e, c)), Skip))
    | Seq(c1, c2) ->
      let s1 = interpret_cmd s c1 in
      interpret_cmd s1 c2
  end

(* optimizations ------------------------------------------------------------ *)

let rec loop : cmd =
  WhileNZ (Lit 1, Skip)


let rec optimize_cmd (c:cmd) : cmd = 
  match c with
  | Assn(x, Var y) -> if x = y then Skip else c
  | Assn(_, _) -> c
  | WhileNZ (Lit 0, c) -> Skip
  | WhileNZ(Lit _, c) -> loop
  | WhileNZ(e, c) -> WhileNZ(e, optimize_cmd c)
  | Skip -> Skip
  | IfNZ(Lit 0, c1, c2) -> optimize_cmd c2
  | IfNZ(Lit _, c1, c2) -> optimize_cmd c1
  | IfNZ(e, c1, c2) -> IfNZ(e, optimize_cmd c1, optimize_cmd c2)
  | Seq(c1, c2) ->
    begin match (optimize_cmd c1, optimize_cmd c2) with
      | (Skip, c2') -> c2'
      | (c1', Skip) -> c1'
      | (c1', c2') -> Seq(c1', c2')
    end



(* We can write a program that runs the SIMPLE factorial program like this: *)
let main () =
  let s_ans = interpret_cmd init_state factorial in
  Printf.printf "ANS = %d\n" (lookup s_ans "ANS")

;; main ()

