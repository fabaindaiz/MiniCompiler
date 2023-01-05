(** Lib **)
open Printf
open Asm
open Ast
open Lib


(* calculate an aprox number of used stack spaces *)
let rec num_expr (expr : tag eexpr) : int =
  1 + (* to store return address in error check call *)
  match expr with
  | ENum (_, _) -> 0
  | EBool (_, _) -> 0
  | ETuple (elist, _) -> 1 + (num_expr_list elist)
  | EId (_, _) -> 0
  | EPrim1 (_, e1, _) -> (num_expr e1)
  | EPrim2 (_, e1, e2, _) -> 1 + (max (num_expr e1) (num_expr e2))
  | ELet (_, e1, e2, _) -> 1 + (max (num_expr e1) (num_expr e2))
  | EIf (c, e1, e2, _) -> (max (num_expr c) (max (num_expr e1) (num_expr e2)))
  | EApp (_, elist, _) -> (num_expr_list elist)
  | ESet (t, n, v, _) -> 2 + (max (num_expr t) (max (num_expr n) (num_expr v)))
  | ELambda (par, _ , _) -> (num_expr_args (List.length par)) (* body se ejecuta despues de actualizar RBP *)
  | ELamApp (fe, ael, _) -> 1 + max (num_expr fe) (num_expr_list ael)
  | ELetRec (recs, body, _) -> (List.length recs) + max (num_expr_recs recs) (num_expr body)
  and num_expr_args (arg_num : int) : int =
    if arg_num >= 7 then arg_num - 6 else 0
  and num_expr_list (elist: tag eexpr list) : int =
    match elist with
    | [] -> 0
    | e1::tail -> (max (num_expr e1) (num_expr_list tail))
  and num_expr_recs (elist: (string * string list * 'a eexpr * 'a) list) : int =
    match elist with
    | [] -> 0
    | (_, _, e1, _)::tail -> (max (num_expr e1) (num_expr_recs tail))


(* pack the closure arguments *)
let closure_pack (l : string list) (reg : reg) (env : renv) : instruction list =
  let len = (Int64.of_int (List.length l)) in
  [ IMovq (RegOffset(reg, 2), Const len) ] @
  let rec update_reg_help (l : string list) (reg : reg) (env : renv) (count : int) : instruction list =
    (match l with
    | [] -> []
    | id::tail ->
      (match (List.assoc_opt id env) with
      | Some arg ->
        [ IMov (Reg R11, arg) ; IMov (RegOffset(reg, count), Reg R11) ] @
        (update_reg_help tail reg env (count+1))
      | None -> failwith (sprintf "unbound variable %s in renv" id) ) ) in
  (update_reg_help l reg env 3)

(* unpack the closure arguments *)
let closure_unpack (l : string list) : instruction list =
  [ ICom ("unpack closure") ] @ [ ISub (Reg RSP, rsp_offset ((List.length l) * 8)) ] @
  [ IMov (Reg R11, Reg RAX) ] @ (* untagged lambda should be already in RAX *)
  let rec unpack_reg_help (l : string list) (count1 : int) (count2 : int): instruction list =
    (match l with
    | [] -> []
    | _::tail ->
        [ IMov (Reg RAX, RegOffset(R11, count1)) ; IMov (RegOffset(RBP, count2), Reg RAX) ] @
        (unpack_reg_help tail (count1 +1) (count2 -1))  ) in
  (unpack_reg_help l 3 (-1))

(* update lambda enviroment with closure arguments *)
let closure_env (l : string list) (env : renv) : renv =
  let rec update_env_help (l : string list) (env : renv) (count : int) : renv =
    (match l with
    | [] -> env
    | id::tail ->
      let env' = (List.remove_assoc id env) in 
      (update_env_help tail ((id, RegOffset(RBP, count))::env') (count-1)) ) in
  (update_env_help l env (-1))


let remove_list_duplicates (xs : 'a list) : 'a list = 
  let cons_uniq (xs : 'a list) (x : 'a) : 'a list = if List.mem x xs then xs else x :: xs in
  List.rev (List.fold_left cons_uniq [] xs)


(* get a list of free vars *)
let get_free_vars (e : tag eexpr) (l : string list) : string list =
  let rec get_free_vars_help (e : tag eexpr) (l : string list) : string list =
    match e with
    | ENum (_, _) -> []
    | EBool (_, _) -> []
    | ETuple (elist, _) -> (get_free_vars_help_list elist l)
    | EId (s, _) ->
      (match List.mem s l with
      | true -> []
      | false -> [s] )
    | EPrim1 (_, e1, _) -> (get_free_vars_help e1 l)
    | EPrim2 (_, e1, e2, _) -> (get_free_vars_help e1 l) @ (get_free_vars_help e2 l)
    | ELet (id, e, body, _) ->
      let l' = l @ [id] in
      (get_free_vars_help e l) @ (get_free_vars_help body l')
    | EIf (c, t, e, _) -> (get_free_vars_help c l) @ (get_free_vars_help t l) @ (get_free_vars_help e l)
    | EApp (_, p, _) -> (get_free_vars_help_list p l)
    | ESet (t, n, v, _) -> (get_free_vars_help t l) @ (get_free_vars_help n l) @ (get_free_vars_help v l)
    | ELambda (par, body, _) ->
      let l' = l @ (get_vars_list par l) in
      (get_free_vars_help body l')
    | ELamApp (fe, ael, _) -> (get_free_vars_help fe l) @ (get_free_vars_help_list ael l)
    | ELetRec (recs, body, _) ->
      let vars = (get_free_vars_help_recs recs l) in
      let names = get_letrec_names recs in
      vars @ (get_free_vars_help body (names @ l)) (* body tiene acceso a nombres de lambdas *)
    and get_free_vars_help_list (elist: tag eexpr list) (l : string list) : string list =
      match elist with
      | [] -> []
      | e1::tail -> (get_free_vars_help e1 l) @ (get_free_vars_help_list tail l)
    and get_letrec_names (elist : (string * string list * 'a eexpr * 'a) list) : string list =
      match elist with
      | [] -> []
      | (name, _, _, _)::tail -> [name] @ get_letrec_names tail
    and get_free_vars_help_recs (elist : (string * string list * 'a eexpr * 'a) list) (l : string list) : string list =
      match elist with
      | [] -> []
      | (_, args, e1, _)::tail ->
          let l' = args @ get_letrec_names elist @ l in (* name and params of letrec's lambdas should not be free vars *)
          (get_free_vars_help e1 l') @ (get_free_vars_help_recs tail l)
    and get_vars_list (slist: string list) (l : string list) : string list =
      match slist with
      | [] -> []
      | s::tail -> [s] @ (get_vars_list tail l) in
  (remove_list_duplicates (get_free_vars_help e l))


(* Compila las clausuras y genera el ambiente para los lambdas de un letrec *)
let rec letrec_env (elist : (string * string list * 'a eexpr * 'a) list) (env : renv) : (renv * instruction list) =
  match elist with
  | [] -> (env, [])
  | (name, params, body, tag)::tail ->
    let (env', reg_offset) = (extend_renv name env) in
    let (env'', instrs) = (letrec_env tail env') in

    let expr = (ELambda (params, body, tag)) in
    let free_vars = (get_free_vars expr []) in
    let heap_offset = (Int64.of_int (((List.length free_vars) + 3) * 8)) in

    (env'' , [ IMov (Reg RAX, Reg R15) ; IAdd (Reg RAX, Const closure_tag) ] @
    [ IMov (RegOffset(RBP, reg_offset), Reg RAX) ; IAdd (Reg R15, Const heap_offset) ] @ instrs)


let get_overwrite_reg (instrs : instruction list) : arg list =
  let rec get_overwrited_reg_help (instrs : instruction list) (l : arg list) : arg list =
    match instrs with
    | [] -> []
    | instr::tail ->
      (match instr with
      | IMov(Reg RAX, x) ->
        (match List.mem x l with
        | true -> [x]
        | false -> [] ) @
        (get_overwrited_reg_help tail l)
      | IMov(x, _) ->
        let l' = l @ [x] in
        (match x with
        | Reg R9 | Reg R8 | Reg RCX | Reg RDX | Reg RDI | Reg RSI -> (get_overwrited_reg_help tail l')
        | _ -> (get_overwrited_reg_help tail l) )
      | _ -> (get_overwrited_reg_help tail l) ) in
  (remove_list_duplicates (get_overwrited_reg_help instrs []))

let function_env (l : arg list) (env : renv) =
  let rec function_env_help (l : arg list) (env : renv) : renv =
    match env with
    | [] -> []
    | (id, reg)::tail ->
      (match List.mem reg l with
      | true ->
        (match reg with
        | Reg R9 -> [(id, RegOffset(RSP, 5))]
        | Reg R8 -> [(id, RegOffset(RSP, 4))]
        | Reg RCX -> [(id, RegOffset(RSP, 3))]
        | Reg RDX -> [(id, RegOffset(RSP, 2))]
        | Reg RSI -> [(id, RegOffset(RSP, 1))]
        | Reg RDI -> [(id, RegOffset(RSP, 0))]
        | _ -> [(id, reg)] ) @ (function_env_help l tail)
      | false ->
        [(id, reg)] @ (function_env_help l tail) ) in
  (function_env_help l env)
