(* CIS 341 *)
(* tc.ml   *)
(* Example code about type checking *)

module Fun = struct 
  type var = string
 
  (* types for our language: note the analogy with values *)
  type tp =
    | IntT                  (* int        the type of integers *)
    | FunT of tp * tp       (* ty1 -> ty2 the type of functions *)
    

  (* Abstract syntax of "typed lambda terms" *)
  type exp = 
    | Int of int              
    | Add of exp * exp
    | Var of var                (* local variables *)
    | Fun of var * tp * exp     (* functions:  fun (x:t) -> e *)
    | App of exp * exp          (* function application *)

  type value =
    | IntV of int
    | FunV of var * tp * exp

  (* values are just a subset of expressions *)
  let exp_of_value v = 
    match v with
      | IntV i -> Int i
      | FunV (arg, t, body) -> Fun(arg, t, body)

  (* An example well-typed term: *)
  (* ( ( (fun (x:int) -> fun (y:int) -> x + y) 3 ) 4 ) *)
  let ex1 = App (App (Fun ("x", IntT, Fun ("y", IntT, Add (Var "x", Var "y"))), 
                     Int 3), 
                Int 4)

  (* An example ill-typed term: *)
  (*   3 + (fun (x:int) -> x)  *)
  let ex2 = Add (Int 3, Fun("x", IntT, Var "x"))

  (* Another example ill-typed term: *)
  (* (fun (x:int -> int) -> x) 3 *)
  let ex3 = App (Fun("x", FunT(IntT, IntT), App(Var "x", Int 4)), Int 3)

  (* An example of conservativity.  This term does *not* *)
  (* cause an exception when evaluated, but it *doesn't* *)
  (* typecheck.*)
  let ex4 = App (Fun ("x", IntT, Var "x"), Fun ("x", IntT, Var "x"))

end

(* This is the closure-based interpreter from fun.ml *)
module Eval3 = struct
  open Fun

  (* Change the definition of value... *)
  type value =
    | IntV of int
    | Closure of environment * var * exp
  and environment = (var * value) list
      
  let lookup x (env:environment) = List.assoc x env

  (* An environment-based interpreter *)
  let rec eval env e =
    match e with 
      | Int i -> IntV i
      | Add (e1, e2) ->
	  (match (eval env e1, eval env e2) with
	     | (IntV i1, IntV i2) -> IntV (i1 + i2)
	     | _ -> failwith "tried to add non-integers")
      | Var x -> lookup x env
      | Fun (arg, t, body) -> Closure(env, arg, body)
      | App (e1, e2) ->
	  (match(eval env e1, eval env e2) with
	     | (Closure(cenv, x, body), v) -> eval ((x,v)::cenv) body 
	     | _ -> failwith "tried to apply non-function")

end

(* This module implements a simple type checker for the typed Fun 
 * language *)
module TC = struct
  open Fun

  type environment = (var * tp) list

  let lookup x (env:environment) = List.assoc x env

  (* Compute the type of a exp, raising an error if there is
   * no good type for the exp *)

  let rec typecheck (env:environment) (e:exp) : tp =
    match e with
      | Int i -> IntT
      | Add (e1, e2) ->
	  (match (typecheck env e1, typecheck env e2) with
	     | (IntT, IntT) -> IntT
	     | _ -> failwith "tried to add non-integers")
      | Var x -> lookup x env
     
        (* Note the correspondence between building a closure
           and typechecking the function body. *)
      | Fun (arg, t, body) -> FunT(t, typecheck ((arg,t)::env) body)

      | App (e1, e2) ->
	  (match(typecheck env e1, typecheck env e2) with
	     | (FunT(t1, t2), t3) -> 
		 if t1 = t3 then t2 
		 else failwith "function argument mismatch"
	     | _ -> failwith "tried to apply non-function")

  (* Check that there is some type for the expression e in the empty
  context. *)
  let well_typed e = 
    ignore (typecheck [] e) 

end
