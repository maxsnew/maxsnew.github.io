(* Translate a simple imperative program into OCaml code by 
 * representing the OCaml program as a string.
 *)

;; open Simple

module OrderedVar = struct
  type t = var
  let compare = String.compare
end

module VSet = Set.Make(OrderedVar)
let (++) = VSet.union

let rec exp_vars (e:exp) : VSet.t =
  match e with
  | Var x -> VSet.singleton x
  | Add (e1, e2) | Mul (e1, e2) | Lt (e1, e2) -> (exp_vars e1) ++ (exp_vars e2)
  | Lit _ -> VSet.empty

let rec cmd_vars (c:cmd) : VSet.t =
  match c with
  | Skip -> VSet.empty
  | Assn (x, e) -> (exp_vars e) ++ (VSet.singleton x)
  | IfNZ (e, c1, c2) -> (exp_vars e) ++ (cmd_vars c1) ++ (cmd_vars c2)
  | WhileNZ (e, c) -> (exp_vars e) ++ (cmd_vars c)
  | Seq (c1, c2) -> (cmd_vars c1) ++ (cmd_vars c2)


let trans_var (x:var) : string =
  "v_" ^ x

let rec trans_exp (e:exp) : string =
  let trans_op e1 op e2 =
    Printf.sprintf "(%s %s %s)" (trans_exp e1) op (trans_exp e2)
  in
  match e with
  | Var x -> "!" ^ (trans_var x)
  | Add (e1, e2) -> trans_op e1 "+" e2
  | Mul (e1, e2) -> trans_op e1 "*" e2
  | Lt  (e1, e2) -> Printf.sprintf "(if %s < %s then 1 else 0)" (trans_exp e1) (trans_exp e2)
  | Lit n -> Printf.sprintf "%d" n
  
let rec trans_cmd (c:cmd) : string =
  match c with
  | Skip -> "()"
  | Assn (x, e) -> Printf.sprintf "%s := %s" (trans_var x) (trans_exp e)
  | IfNZ (e, c1 ,c2) ->
    Printf.sprintf "if %s <> 0 then %s else %s"
      (trans_exp e)
      (trans_cmd c1)
      (trans_cmd c2)
  | WhileNZ (e, c) ->
    Printf.sprintf "while %s <> 0 do\n %s\ndone" (trans_exp e) (trans_cmd c)
  | Seq (c1, c2) ->
    Printf.sprintf "%s;\n%s" (trans_cmd c1) (trans_cmd c2)

let trans_prog (c:cmd) : string =
  let vars = cmd_vars c in
  let decls =
    VSet.fold (fun x s -> Printf.sprintf "let %s = ref 0\n%s" (trans_var x) s)
      vars ""
  in
  Printf.sprintf "module Program = struct\n%slet run () = \n%s\nend" decls (trans_cmd c)
