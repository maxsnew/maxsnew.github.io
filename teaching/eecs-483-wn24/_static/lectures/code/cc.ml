open Fun

(* A "closure-based" intermediate language based on Eval3 *)
module IR = struct

  (* An intermediate language with enough data types to represent
     meta-level environments as IL-level values.  

     - we need a way to represent closures at the "object level"
       In OCaml, we have:

       (* Change the definition of value... *)
       type value =
       | IntV of int
       | Closure of environment * var * exp
       and environment = (var * value) list

     - So: we need a way to represent tuples and lists.

       Observe that at every point where we build a closure,
       we know its contents, so we can use tuples there too.

       What about variables?  The compiler can _statically_ determine
       a mapping from variable names to tuple offsets.

     - Add: N-ary tuples and projection operations.

     Once the code is hoisted to the top level, we need a notion of a
     global variable (which at the assembly level becomes a label for
     the function body code.)

     The IR also needs local variable definitions.  Here, we choose to
     use our familiar 'let' expressions. 

     - Every function takes exactly two inputs: 
         (1) the environment and (2) the "usual" function argument
       SO: build in two-ary functions
  *)

  type var = string
  type exp =
    | Val of value             (* all values are expressions *)
    | Var of var               (* local variables *)
    | Add of exp * exp         (* binary operations *)
    | App of exp * exp         (* application *)
    | Tuple of exp list
    | Nth of exp * int

    | Let of var * exp * exp   (* introduce local variables *)
    | Global of var            (* global variables *)


  and value =
    | IntV of int
    | CodeV of var * var * exp   (* Environment name, arg name, body *)
    | TupleV of value list

	  
  type environment = var list

  (* Generates fresh variable names *)
  let mk_tmp =
    let ctr = ref 0 in 
      fun hint ->
      let x = !ctr in ctr := x + 1; (hint ^ string_of_int(x))



  (*************************************)
  (* The main closure conversion code. *)
  (*************************************)

  (* A function prologue that sets up the expected local variables. *)
  let build_local_env (env:environment) (env_name:var) (body:exp) : exp =
    let (_, code) = 
      List.fold_left (fun (i, code) -> fun x ->
                        (i+1, Let(x, Nth(Var env_name, i), code))) 
           (0, body) env
    in
      code

  (* Here is where we convert he "meta-level" environment to an
     "object-level" data structure.  Here, an OCaml list is converted
     to an IL tuple.  In practice, this could use other data
     structures or store only the free variables in the function.  If
     so, then build_local_env would be modified appropriately. *)
  let build_closure_env env =
    Tuple (List.map (fun x -> Var x) env)

  let rec convert (env:environment) (e:Fun.exp) : exp =
    match e with
      | Fun.Int i -> Val (IntV i)
      | Fun.Add (e1, e2) -> Add (convert env e1, convert env e2)
      | Fun.Var x -> Var x
      | Fun.Fun (arg, body) -> 
	  let env_name = mk_tmp "ENV" in
	  let body' = build_local_env env env_name (convert (arg::env) body) in
	  let env' = build_closure_env env in
	    Tuple [env'; Val(CodeV(env_name, arg, body'))]

      | Fun.App (e1, e2) -> App (convert env e1, convert env e2)

  (* Top-level programs are closure-converted starting in an empty environment *)
  let closure_convert = convert [] 



  (* Assumes that all code is closed and moves it to a top level
     definition of a global variable.  

     In practice, hoisting and closure conversion could be combined
     into one pass. *)
  let hoist e =
    let rec hoist_exp (e:exp):((var * value) list * exp) =
      match e with
	| Val(CodeV(env, x, body)) -> 
	    let (c1, r1) = hoist_exp body in
	    let tmp = mk_tmp "CODE" in
	      ((tmp, CodeV(env, x, r1))::c1, Global tmp)
	| Val(v) -> 
	    let (c1, r1) = hoist_val v in
	      (c1, Val r1)
	| Var x -> ([], Var x)
	| Global x -> ([], Global x)
	| Add(e1, e2) ->
	    let (c1, r1) = hoist_exp e1 in
	    let (c2, r2) = hoist_exp e2 in
	      (c1@c2, Add(r1, r2))
	| App(e1, e2) ->
	    let (c1, r1) = hoist_exp e1 in
	    let (c2, r2) = hoist_exp e2 in
	      (c1@c2, App(r1, r2))
	| Let(x, e1, e2) ->
	    let (c1, r1) = hoist_exp e1 in
	    let (c2, r2) = hoist_exp e2 in
	      (c1@c2, Let(x, r1, r2))
	| Tuple(elist) ->
	    let (cs, rs) = List.split(List.map hoist_exp elist) in
	      (List.concat cs, Tuple rs)
	| Nth(e1, i) ->
	    let (c1, r1) = hoist_exp e1 in
	      (c1, Nth(r1, i))

    and hoist_val v =
      match v with
	| IntV i -> ([], IntV i)
	| TupleV(vlist) ->
	    let (cs, rs) = List.split(List.map hoist_val vlist) in
	      (List.concat cs, TupleV rs)
	| _ -> failwith "impossible"
    in	    
      hoist_exp e

  (* Assumes that Code is closed *)
  let rec subst v x e =
    match e with 
      | Val u -> Val u     (* Note that we don't substitute through values *)
      | Var y -> if y = x then Val v else e
      | Global x -> Global x
      | Add(e1, e2) -> Add(subst v x e1, subst v x e2)
      | App(e1, e2) -> App(subst v x e1, subst v x e2)
      | Let(y, e1, e2) ->
	  let e1' = subst v x e1 in
	    if x = y then Let(y, e1', e2) else Let(y, e1', subst v x e2)
      | Tuple(elist) -> Tuple(List.map (subst v x) elist)
      | Nth(e, i) -> Nth(subst v x e, i)

  
  let rec eval (globals, e) =
    let rec eval_e e = 
      match  e with
	| Var x -> failwith "tried to evaluate free variable"
	| Global x -> List.assoc x globals
	| Val u -> u
	| Add(e1, e2) -> 
	    (match (eval_e e1, eval_e e2) with
	       | (IntV i1, IntV i2) -> IntV (i1 + i2)
	       | _ -> failwith "tried to add two non-integers")

	(* Since closures are pairs, application expects to find a tuple. *)
        (* Since all function code takes two parameters, this interpreter *)
        (* "hardwires" the function call.*)
	| App(e1, e2) -> 
	    (match (eval_e e1, eval_e e2) with
	       | (TupleV [env; CodeV(env_name, x, b)], v) -> 
		   eval_e (subst v x (subst env env_name b))
	       | _ -> failwith "tried to apply non-closure") 

	| Let(x, e1, e2) -> eval_e (subst (eval_e e1) x e2)
	| Tuple(elist) -> TupleV(List.map eval_e elist)
	| Nth(e, i) -> 
	    (match  eval_e e with
	       | TupleV vlist -> List.nth vlist i
	       | _ -> failwith "tried to project from non tuple")
    in eval_e e




  (* Pretty printing of IR terms *)
  and pp_e e : string =
    match e with
      | Val v -> pp_v v
      | Var v -> v
      | Global v -> v
      | Add (e1, e2) -> Printf.sprintf "%s + %s" (pp_e e1) (pp_e e2)
      | App (e1, e2) -> Printf.sprintf "APPLY(%s %s)" (pp_e e1) (pp_e e2)
      | Let (v, e1, e2) -> Printf.sprintf "let %s = %s in %s" v (pp_e e1) (pp_e e2)
      | Tuple elist -> "<"^(String.concat ", " (List.map pp_e elist)) ^ ">"
      | Nth (e, i) -> Printf.sprintf "nth(%s, %d)" (pp_e e) i 
  and pp_v v : string =
    match v with
      | IntV i -> string_of_int i
      | CodeV(env, x, body) -> Printf.sprintf "(fun (%s, %s) -> %s)" env x (pp_e body)
      | TupleV vlist -> "(" ^ (String.concat ", " (List.map pp_v vlist)) ^")"
	  
  let pp_globals g =
    String.concat "\n" (List.map (fun (v, def) -> Printf.sprintf "%s := %s" v (pp_v def)) g)
	 
  let pp_top (g, e) =
    print_string (Printf.sprintf "%s\n%s\n" (pp_globals g) (pp_e e))
end

(*
  (* ( ( fun f -> ((fun x -> f 1) 4) )
       ( ( fun x -> fun y -> x + y ) 2) ) *)
  let dynamic_scope_example = 
  App(Fun ("f", App( Fun ("x1", App(Var "f", Int 1)), Int 4)),
       App(Fun ("x1", (Fun ("y", Add (Var "x1", Var "y")))), Int 2))




*)
