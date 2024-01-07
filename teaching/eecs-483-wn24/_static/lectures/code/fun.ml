(* CIS 341 *)
(* fun.ml  *)
(* Example code about interpretation and compilation of first class 
   (higher-order) functions. *)

(* A simple set implementation *)
module VarSet : sig
  type t
  val empty : t
  val union : t -> t -> t
  val singleton : string -> t
  val member : string -> t -> bool
  val remove : string -> t -> t
  val equals : t -> t -> bool
end = struct
  type t = string list
  let empty = []      
  let union s1 s2 = s1 @ s2
  let singleton x = [x]
  let rec member x s =
    match s with
    | [] -> false
    | y::ys -> x = y || member x ys
  let rec remove x s =
    match s with
    | [] -> []
    | y::ys -> if x = y then remove x ys else y::(remove x ys)

  let rec forall p s =
    List.fold_left (fun b x -> b && (p x)) true s
    
  let equals s1 s2 =
    (forall (fun x -> member x s1) s2) &&
    (forall (fun x -> member x s2) s1)

end


module Fun0 = struct 
  type var = string
 
  (* Abstract syntax of "lambda terms" *)
  type exp = 
    | Var of var                (* local variables *)
    | Fun of var * exp          (* functions:  fun x -> e *)
    | App of exp * exp          (* function application: e1 e2 *)


  (* The only values of the pure, untyped lambda calculus are (closed) functions. *)
  (* We make a type distinction here so that we can compare to later implementations. *)
  type value =
    | FunV of var * exp

  (* values are just a subset of expressions *)
  let exp_of_value (v:value) : exp = 
    match v with
      | FunV (arg, body) -> Fun(arg, body)

  
  (* Functions for working with variable names and alpha conversion *)
  let rec free_vars (e:exp) : VarSet.t =
    match e with
    | Var x      -> VarSet.singleton x
    | App(e1,e2) -> VarSet.union (free_vars e1) (free_vars e2)
    | Fun(x, e)  -> VarSet.remove x (free_vars e)


  let rec fresh_var_for (s:VarSet.t) (x:var) : var =
    if VarSet.member x s then
      fresh_var_for s (x ^ "'")
    else
      x

  (* rename all _free_ occurrences of x to y in e *)
  let rec rename x y e : exp =
    match e with
    | Var z -> if z = x then Var y else Var z
    | App(e1, e2) -> App(rename x y e1, rename x y e2)
    | Fun(z, e) -> if z = x then Fun(z, e) else Fun(z, rename x y e)
  

end

module Examples0 = struct
  open Fun0

  (* Example untyped lambda calculus terms *)
  let id_tm = Fun("x", Var "x")      (* fun x -> x *)
  let ex = App(id_tm, id_tm)         (* (fun x -> x) (fun x -> x) *)

  
  (* The omega term -- demonstrates an infinite loop written in
     untyped lambda calculus. 

       (fun x -> x x) (fun x -> x x)
   =>  (x x){(fun x -> x x)/x} 
   ==  (fun x -> x x) (fun x -> x x)
  *)
  let omega =
    App(Fun ("x", App(Var "x", Var"x")),
	Fun ("x", App(Var "x", Var"x")))

  
  (* The "Y Combinator" -- not a startup incubator!  It calculates
     fixedpoints of a function f 

    Y == (fun f -> (fun x -> f (x x)) (fun x -> f (x x)))

    Y g = (fun x -> g (x x))(fun x -> g (x x)) =>
         g ((fun x -> g (x x)) (fun x -> g (x x))) =>
         g (g ((fun x -> g (x x)) (fun x -> g (x x)))) =>
         g (g (g ((fun x -> g (x x)) (fun x -> g (x x))))) =>
         ...
  *)
  let y_combinator =
    Fun ("FX", App(Fun("x", App(Var "FX", App(Var "x", Var "x"))),
  		   Fun("x", App(Var "FX", App(Var "x", Var "x")))))
    

  let cbv_y =
    Fun ("f", App(Fun("x", App(Var "f", Fun("v", App(App(Var "x", Var "x"), Var "v")))),
		  Fun("x", App(Var "f", Fun("v", App(App(Var "x", Var "x"), Var "v"))))))
    
  let true_tm  = Fun("tru", Fun("fls", Var "tru"))
  let false_tm = Fun("tru", Fun("fls", Var "fls"))
  (* 
      ite b e1 e2 ==> e1   if b ==> true
      ite b e1 e2 ==> e2   if b ==> false
  *)
  let if_then_else b e1 e2 = App(App(b, e1), e2)
  
  (* Church encoding of the natural numbers *)
  let zero  = Fun("f", Fun("y", Var "y"))
  let one   = Fun("f", Fun("y", App(Var "f", Var "y")))
  let two   = Fun("f", Fun("y", App(Var "f", App(Var "f", Var "y"))))
  let three = Fun("f", Fun("y", App(Var "f", App(Var "f", App(Var "f", Var "y")))))
  let mk_n n =
    let rec apps n = if n = 0 then Var "y" else App(Var "f", apps (n-1)) in
    Fun("f", Fun("y", apps n))

  
  (* Arithmetic operations *)
  (* add = fun x1 -> fun x2 -> fun f -> fun y -> (x1 f) ((x2 f) y)  *)
  let add = Fun("x1", Fun("x2", Fun("f", Fun("y", App(App(Var "x1", Var "f"), App(App(Var "x2", Var "f"), Var "y"))))))
  let add' x y = App(App(add, x), y)
  let test_add = add' two three
  
  (* mult =  fun x1 -> fun x2 -> fun f -> fun y -> (x1 (x2 f)) y *)
  let mult =  Fun("x1", Fun("x2", Fun("f", Fun("y", App(App(Var "x1", App(Var "x2", Var "f")), Var "y")))))
  let mult' x y = App(App(mult, x), y)
  let test_mult = mult' two three

  let pair = Fun("x", Fun("y", Fun("pair", App(App(Var "pair", Var "x"), Var "y"))))
  let pair' x y = App(App(pair, x), y)
  let fst = Fun("p", App(Var "p", true_tm))
  let snd = Fun("p", App(Var "p", false_tm))

  let test_pair_fst = App(fst, pair' one two)
  let test_pair_snd = App(snd, pair' one two)
  
  let add_one = App(add, one)
  let shift = Fun("p", App(App(pair, (App(snd, (Var "p")))), (App(add_one, App(snd, (Var "p"))))))
  let pred = Fun("x", Fun("f", Fun("y", App(App(App(fst, App(App(Var "x", shift), App(App(pair, zero), zero))), Var "f"), Var "y"))))
  let pred' x = App(pred, x)
  
  let test_pred = pred' three

  (* if0 v e1 e2 ==> e1  if v ==> zero and if0 v e1 e2 ==> e2 if v ==> non-zero *)
  let if0 = Fun("x", Fun("e1", Fun("e2", App(App(Var "x", Fun("w", Var "e2")), Var "e1"))))

  let if0' v e1 e2 = App(App(App(if0, v), e1), e2)
  let test_if0_0 = if0' zero one two
  let test_if0_1 = if0' one one two
  let test_if0_2 = if0' two one two

  let fix = y_combinator

  (* 
     fun F -> fun x -> if0 x 1 (mult x (F (pred x)))
     factorial = Y F
  *) 

  (* fun LOOP -> fun x -> if0 x 1 (LOOP (pred x)) *)
  let fLOOP = Fun("LOOP", Fun("x", App(App(App(if0, Var "x"), one), (App(Var "LOOP", App(pred, Var "x"))))))
  let loop = App(fix, fLOOP)
  
  
  let factF = Fun("FACT",
                  Fun("x",
                      if0' (Var "x")
                        one
                        (mult' (Var "x") (App(Var "FACT", pred' (Var "x"))))))
                      
  let fact = App(fix, factF)
  let fact3 = App(fact, three)
  
end
  


(* A substitution-based evaluator *)
module Eval0 = struct
  open Fun0

  
  (* Substitute the (closed) value v for x in e *)
  let rec subst (v:value) (x:var) (e:exp) : exp =
    match e with
      | Var y -> if y = x then exp_of_value v else Var y
      | Fun (arg, body) -> 
	  if (arg = x) then Fun (arg, body) 
	  else Fun(arg, subst v x body)
      | App (e1, e2) -> App(subst v x e1, subst v x e2)

  
  (* A call-by-value substituion-based interpreter - assumes e is closed 

     Compare this code to the large-step operational semantics rules:

       -----------
        v  ==>  v 

 
       e1 ==> (fun x -> e3)    e2 ==> v     e3{v/x} ==> w
      ----------------------------------------------------     
       e1 e2 ==> w
  *)
  let rec cbv (e:exp) : value =
    match e with
      | Var x -> failwith "free variable: impossible if e is closed"
      | Fun (arg, body) -> FunV (arg, body)
      | App (e1, e2) -> 
	  (match(cbv e1, cbv e2) with
	     | (FunV (x, e3), v) -> cbv (subst v x e3))


  (* Proper capture-avoiding subsitution. *)
  let rec subst' (e1 : exp) (x:var) (e2 : exp) : exp =
    match e2 with
      | Var y -> if y = x then e1 else Var y
      | Fun (arg, body) -> 
        if arg = x then Fun(arg, body)
        else
          let arg' = fresh_var_for (free_vars e1) arg in
          Fun(arg', subst' e1 x (rename arg arg' body))
      | App (e21, e22) -> App(subst' e1 x e21, subst' e1 x e22)

  let rec cbv' (e : exp) : value =
    match e with
      | Var x -> failwith "free variable: impossible if e is closed"
      | Fun (arg, body) -> FunV (arg, body)
      | App (e1, e2) -> 
	  (match(cbv' e1, cbv' e2) with
	     | (FunV (x, e3), v) -> cbv' (subst' (exp_of_value v) x e3))

  (* A call-by-name substituion-based interpreter - assumes e is closed 

     Compare this code to the large-step operational semantics rules:

     -----------
     v  ==>  v 


     e1 ==> (fun x -> e3)      e3{e2/x} ==> w
     ----------------------------------------------------     
     e1 e2 ==> w
  *)
  let rec cbn (e : exp) : value =
    match e with
    | Var x -> failwith "free variable: impossible if e is closed"
    | Fun (arg, body) -> FunV (arg, body)
    | App (e1, e2) -> 
      begin match (cbn e1) with
        | FunV (x, e3) -> cbn (subst' e2 x e3)
      end

  (* reduces lambda calculus terms everywhere, even within the body of a
     function.  Can easily go into an infinite loop. *)
  let rec reduce (e : exp) : exp =
    match e with
    | Var x -> Var x
    | Fun (arg, body) -> Fun (arg, reduce body)
    | App (e1, e2) ->
      (match (reduce e1, reduce e2) with
       | (Var x, n2) -> App (Var x, n2)
       | (Fun (x, body), v) -> reduce (subst' v x body)
       | _ -> App(e1, e2))


  (* "weak head-normal form" simplifies lambda terms lazily until it gets stuck on a variable 
      application
  *)
  let rec whnf (e : exp) : exp =
    match e with
    | Var x -> Var x
    | Fun (arg, body) -> Fun (arg, body)
    | App (e1, e2) ->
      (match whnf e1 with
       | Var x -> App (Var x, e2)
       | Fun (x, body) -> whnf (subst' e2 x body)
       | _ -> App(e1, e2))
  
end



(* Lambda calculus with built in integers --------------------------------- *)
module Fun = struct 
  type var = string
 
  (* Abstract syntax of "lambda terms" *)
  type exp = 
      Int of int              
    | Add of exp * exp
    | Var of var                (* local variables *)
    | Fun of var * exp          (* functions:  fun x -> e *)
    | App of exp * exp          (* function application *)

  type value =
    | IntV of int
    | FunV of var * exp

  (* values are just a subset of expressions *)
  let exp_of_value v = 
    match v with
      | IntV i -> Int i
      | FunV (arg, body) -> Fun(arg, body)

  let rec free_vars (e:exp) : VarSet.t =
    match e with
    | Int _ -> VarSet.empty
    | Var x -> VarSet.singleton x
    | App(e1,e2)
    | Add(e1, e2) -> VarSet.union (free_vars e1) (free_vars e2)
    | Fun(x, e)   -> VarSet.remove x (free_vars e)

     
end

module Examples = struct
  open Fun

  (* Example term: *)
  (* fun x -> fun y -> x + y *)
  let addc = Fun("x", Fun("y", Add(Var "x", Var "y")))
                                          
  (* (fun x -> fun y -> x + y) 3  *)
  let ex1 = App((addc), Int 3)
  let ex2 = App(ex1, Int 4)


  (* The following example illustrates the difference between dynamically
   * scoped and statically scopes variable binding:
   * - under static scoping (i.e. substitution semantics), it returns
   *   the value 3
   * - under dynamic scoping, it returns the value 5
   *)
  (* ( ( fun f -> ((fun x -> f 1) 4) )
       ( ( fun x -> fun y -> x + y ) 2) ) *)
  let dynamic_scope_example = 
  App(Fun ("f", App( Fun ("x1", App(Var "f", Int 1)), Int 4)),
       App(Fun ("x1", (Fun ("y", Add (Var "x1", Var "y")))), Int 2))


  (* The omega term -- demonstrates an infinite loop written in
     untyped lambda calculus. *)
  let omega =
    App(Fun ("x", App(Var "x", Var"x")),
	Fun ("x", App(Var "x", Var"x")))

  (* The "Y Combinator" -- not a startup incubator!  It calculates
     fixedpoints of a function f *)
  let y_combinator =
    Fun ("f", App(Fun("x", App(Var "f", App(Var "x", Var "x"))),
		  Fun("x", App(Var "f", App(Var "x", Var "x")))))

  (* Church encoding of the natural numbers *)
  let zero = Fun("f", Fun("y", Var "y"))
  let one  = Fun("f", Fun("y", App(Var "f", Var "y")))
  let two  = Fun("f", Fun("y", App(Var "f", App(Var "f", Var "y"))))
  let three = Fun("f", Fun("y", App(Var "f", App(Var "f", App(Var "f", Var "y")))))
  let mk_n n =
    let rec apps n = if n = 0 then Var "y" else App(Var "f", apps (n-1)) in
    Fun("f", Fun("y", apps n))

  (* An arithmetic operation *)
  let add = Fun("x1", Fun("x2", Fun("f", Fun("y", App(App(Var "x1", Var "f"), App(App(Var "x2", Var "f"), Var "y"))))))
  let test_add = App(App(add, two), three)

  let mult =  Fun("x1", Fun("x2", Fun("f", Fun("y", App(App(Var "x1", App(Var "x2", Var "f")), Var "y")))))
  let test_mult = App(App(mult, two), three)
  
  let church_to_int c = App(App(c, Fun("z", Add(Var "z", Int 1))), Int 0)

  let pair x y = Fun("pair", App(App(Var "pair", x), y))
  let fst e = App(e, Fun("x", Fun("y", Var "x")))
  let snd e = App(e, Fun("x", Fun("y", Var "y")))      

end

(* A substitution-based evaluator *)
module Eval1 = struct
  open Fun

  (* Substitute the (closed) value v for x in e *)
  let rec subst (v:value) (x:var) (e:exp) : exp =
    match e with
      | Int i -> Int i
      | Add (e1, e2) -> Add(subst v x e1, subst v x e2)
      | Var y -> if y = x then exp_of_value v else Var y
      | Fun (arg, body) -> 
	  if (arg = x) then Fun (arg, body) 
	  else Fun(arg, subst v x body)
      | App (e1, e2) -> App(subst v x e1, subst v x e2)

  (* A substituion-based interpreter *)
  let rec eval (e : exp) : value =
    match e with
      | Int i -> IntV i
      | Add (e1, e2) -> 
	  (match (eval e1, eval e2) with
	    | (IntV i1, IntV i2) -> IntV (i1 + i2)
	    | _ -> failwith "tried to add non-integers")
      | Var x -> failwith "reached a free variable"
      | Fun (arg, body) -> FunV (arg, body)
      | App (e1, e2) -> 
	  (match(eval e1, eval e2) with
	     | (FunV (x, body), v) -> eval(subst v x body)
	     | _ -> failwith "tried to apply non-function")

end

(* A *broken* attempt to use an environment-based evaluator *)
(* This is broken in the sense that it disagrees with the 
   substitution semantics presented in the Eval1 module. *)
module Eval2 = struct
  open Fun

  type environment = (var * value) list
  let lookup x (env:environment) = List.assoc x env

  (* The following evaluator gives *dynamic* scoping, not
     static (or 'lexical') scoping! *)
  let rec eval (env : environment) (e : exp) : value =
    match e with 
      | Int i -> IntV i
      | Add (e1, e2) ->
	  (match (eval env e1, eval env e2) with
	     | (IntV i1, IntV i2) -> IntV (i1 + i2)
	     | _ -> failwith "tried to add non-integers")
      | Var x -> lookup x env
      | Fun (arg, body) -> FunV(arg, body)
      | App (e1, e2) ->
	  (match(eval env e1, eval env e2) with
	     | (FunV (x, body), v) -> eval ((x,v)::env) body 
	     | _ -> failwith "tried to apply non-function")

end





(* A "closure"-based interpreter that fixes the problem with Eval2 *)
(* A closure is a pair consisting of a datastructure that represents 
   the evaluation context and a function *)
module Eval3 = struct
  open Fun

  (* Change the definition of value... *)
  type value =
    | IntV of int
    | Closure of environment * var * exp
  and environment = (var * value) list
      
  let lookup x (env:environment) = List.assoc x env

  (* The following evaluator fixes the problems with Eval2 *)
  (* It yields statically-scoped  variables *)
  let rec eval (env : environment) (e : exp) : value  =
    match e with 
      | Int i -> IntV i
      | Add (e1, e2) ->
	  (match (eval env e1, eval env e2) with
	     | (IntV i1, IntV i2) -> IntV (i1 + i2)
	     | _ -> failwith "tried to add non-integers")
      | Var x -> lookup x env

      (* NOTE: the closure captures the environment in force at 
         the time that the function is created *)
      | Fun (arg, body) -> Closure(env, arg, body)

      | App (e1, e2) ->
	  (match(eval env e1, eval env e2) with
	     | (Closure(cenv, x, body), v) -> eval ((x,v)::cenv) body 
	     | _ -> failwith "tried to apply non-function")

end
