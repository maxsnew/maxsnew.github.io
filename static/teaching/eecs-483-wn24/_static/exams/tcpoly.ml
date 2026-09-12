module Fun = struct 
  type tvar = string   (* type variables, "alpha" "beta" etc. *)
  type var = string    (* term variables, "x", "y", "f", etc. *)
 
  (* types *)
  type tp =
    | BoolT                 (* the type of booleans *)
    | FunT of tp * tp       (* ty1 -> ty2 the type of functions *)
    | VarT of tvar          (* type variables "alpha", "beta", "gamma" *)
    | GenT of tvar * tp     (* generic type: <a>t *)

  (* Abstract syntax of "polymorphic lambda terms" *)
  type exp = 
    | Lit of bool
    | Var of var                (* local variables *)
    | Fun of var * tp * exp     (* functions:  fun (x:t) -> e *)
    | App of exp * exp          (* function application *)
    | Gen of tvar * exp         (* generic type parameter *)
    | Ins of exp * tp           (* type instantiation *)             

end

(* This module implements a simple type checker for the polymorphic language *)
module TC = struct
  open Fun

  type typ_environment = tvar list
  type environment = (var * tp) list

  let contains (alpha:tvar) (delta:typ_environment) = List.mem alpha delta
  let lookup x (gamma:environment) = List.assoc x gamma

  (* Well-formed types:  returns unit if delta |- t ok  and fails otherwise *)
  let rec wft (delta:typ_environment) (t:tp) : unit =
    begin match t with
      | BoolT -> ()
      | FunT(t1, t2)-> wft delta t1 ; wft delta t2
      | VarT alpha ->
        if contains alpha delta then ()
        else failwith "type variable not in scope"
      | GenT(alpha, t1) ->
        if contains alpha delta
        then failwith "rebound type variable" 
        else wft (alpha::delta) t1
    end

  (* Type substitution: implements t1{t/alpha} *)
  let rec tsubst (t1:tp) (t:tp) (alpha:tvar) : tp =
    begin match t1 with
      | BoolT -> BoolT
      | FunT(t11, t12) -> FunT(tsubst t11 t alpha, tsubst t12 t alpha)
      | VarT (beta) ->
        if alpha = beta then t else VarT(beta)
      | GenT(beta, t11) ->
        if alpha = beta then t1 else GenT(beta, tsubst t11 t alpha)
    end

  (* Compute the type of an exp, raising an error if there is no such type 
     Returns the type t such that  delta ; gamma |- e : t
  *)
  let rec typecheck (delta:typ_environment) (gamma:environment) (e:exp) : tp =
    begin match e with
      | Lit i -> BoolT

      | Var x -> lookup x gamma

      | Fun (arg, t, body) ->
        let _ = wft delta t in 
        FunT(t, typecheck delta ((arg,t)::gamma) body)

      | App (e1, e2) ->
	begin match (typecheck delta gamma e1, typecheck delta gamma e2) with
	  | (FunT(t1, t2), t3) -> 
	    if t1 = t3 then t2 
	    else failwith "function argument mismatch"
          | _ -> failwith "tried to apply non-function"
        end

      | Gen(alpha, e1) ->  failwith "TODO"

      | Ins(e1, t) ->  failwith "TODO"

    end
end (* TC Module *)

