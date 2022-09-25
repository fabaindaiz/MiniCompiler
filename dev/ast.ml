(** AST **)
open Printf


(* primitivas unarias *)
type prim1 =
| Add1
| Sub1
| Not
| Print

(* primitivas binarias *)
type prim2 =
| Add
| Sub
| Mul
| Div
| And
| Or
| Lt
| Gt
| Lte
| Gte
| Eq
| Neq

(* Algebraic datatype for expressions *)
type expr = 
| Num of int64 
| Bool of bool
| Id of string
| Prim1 of prim1 * expr
| Prim2 of prim2 * expr * expr
| Let of string * expr * expr
| If of expr * expr * expr
| App of string * expr list

(* Algebraic tagged datatype for expressions *)
type 'a eexpr =
| ENum of int64 * 'a
| EBool of bool * 'a
| EId of string * 'a
| EPrim1 of prim1 * 'a eexpr * 'a
| EPrim2 of prim2 * 'a eexpr * 'a eexpr * 'a
| ELet of string * 'a eexpr * 'a eexpr * 'a
| EIf of 'a eexpr * 'a eexpr * 'a eexpr * 'a
| EApp of string * 'a eexpr list * 'a

(* C function argument types *)
type ctype =
  | CAny
  | CInt
  | CBool

(* Function definitions *)
(* function name, argument names, body *)
type fundef =
  | DefFun of string * string list * expr
  | DefSys of string * ctype list * ctype

type 'a efundef =
  | EDefFun of string * string list * 'a eexpr * 'a
  | EDefSys of string * ctype list * ctype

let fundef_name(f : fundef) : string =
  match f with
  | DefFun (n, _, _) -> n
  | DefSys (n, _, _) -> n

(* Program including definitions and a body *)
type prog = fundef list * expr

type 'a eprog = 'a efundef list * 'a eexpr


type tag = int

let rec tag_expr_help (e : expr) (cur : tag) : (tag eexpr * tag) =
  match e with
  | Num n ->
    let (tag_n, next_tag) = (cur, cur + 1) in
    (ENum (n, tag_n), next_tag)
  | Bool b ->
    let (tag_b, next_tag) = (cur, cur + 1) in
    (EBool (b, tag_b), next_tag)
  | Id (x) ->
    let (tag_x, next_tag) = (cur, cur + 1) in
    (EId (x, tag_x), next_tag)
  | Prim1 (op, e) ->
    let (tag_e, next_tag) = tag_expr_help e (cur + 1) in
    (EPrim1 (op, tag_e, cur), next_tag)
  | Prim2 (op, e1, e2) ->
    let (tag_e1, next_tag1) = tag_expr_help e1 (cur + 1) in
    let (tag_e2, next_tag2) = tag_expr_help e2 next_tag1 in
    (EPrim2 (op, tag_e1, tag_e2, cur), next_tag2)
  | Let (x, e, b) ->
    let (tag_e, next_tag1) = tag_expr_help e (cur + 1) in
    let (tag_b, next_tag2) = tag_expr_help b next_tag1 in
    (ELet (x, tag_e, tag_b, cur), next_tag2)
  | If (c, t, e) ->
    let (tag_c, next_tag1) = tag_expr_help c (cur + 1) in
    let (tag_t, next_tag2) = tag_expr_help t next_tag1 in
    let (tag_e, next_tag3) = tag_expr_help e next_tag2 in
    (EIf (tag_c, tag_t, tag_e, cur), next_tag3)
  | App (f, p) ->
    let next_tag = ref (cur + 1) in
    let tag_e = List.fold_left (fun res i -> res @
      let (tag_i, temp_tag) = (tag_expr_help i !next_tag) in
        next_tag := temp_tag ;  [ tag_i ] ) [] p in
    (EApp (f, tag_e, cur), !next_tag)

let tag_expr (e : expr) : tag eexpr =
  let (tagged, _) = tag_expr_help e 1 in tagged


let tag_fundef_help (d : fundef) (cur : int) : (tag efundef * tag) =
  match d with
  | DefFun (f, x, e) ->
    let (tag_e, next_tag) = tag_expr_help e (cur + 1) in
    (EDefFun (f, x, tag_e, cur), next_tag)
  | DefSys (f, x, e) ->
    (EDefSys (f, x, e), cur)

let tag_fundef (d : fundef) : tag efundef =
  let (tagged, _) = tag_fundef_help d 1 in tagged


let tag_program_help (p : prog) (cur : int) : (tag eprog * tag) =
  let d, e = p in
    let next_tag1 = ref (cur) in
    let tag_d = List.fold_left (fun res i -> res @
      let (tag_i, temp_tag) = (tag_fundef_help i !next_tag1) in
        next_tag1 := temp_tag ;  [ tag_i ] ) [] d in
    let (tag_e, next_tag2) = tag_expr_help e (!next_tag1) in
    ((tag_d, tag_e), next_tag2)

let tag_program (p : prog) : tag eprog =
  let (tagged, _) = tag_program_help p 1 in tagged


let string_of_elist op expr =
  (List.fold_left (fun res i -> res ^ ", " ^ (op i)) "" expr)

(* Pretty printing - used by testing framework *)
let rec string_of_expr(e : expr) : string = 
  match e with
  | Num n -> Int64.to_string n
  | Bool b -> Bool.to_string b
  | Id s -> s
  | Prim1 (op, e1) -> sprintf "(%s %s)" 
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | Not -> "not"
    | Print -> "print") (string_of_expr e1)
  | Prim2 (op, e1, e2) -> sprintf "(%s %s %s)" 
    (match op with 
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | And -> "and"
    | Or -> "or"
    | Lt -> "<"
    | Gt -> ">"
    | Lte -> "<="
    | Gte -> ">="
    | Eq -> "=="
    | Neq -> "!=") (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
  | If (e1, e2, e3) -> sprintf "(if %s %s %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | App (f, e1) -> sprintf "(app %s (%s))" f (string_of_elist string_of_expr e1)


(** functions below are not used, would be used if testing the parser on defs **)

(* Pretty printing C types - used by testing framework *)
let string_of_ctype(t : ctype) : string =
match t with
| CAny -> "any"
| CInt -> "int"
| CBool -> "bool"

(* Pretty printing function definitions - used by testing framework *)
let string_of_fundef(d : fundef) : string =
  match d with
  | DefFun (name, arg_ids, body) -> sprintf "(def (%s %s) %s)" name (String.concat " " arg_ids) (string_of_expr body)
  | DefSys (name, arg_types, ret_type) -> sprintf "(defsys %s %s -> %s)" name (String.concat " " (List.map string_of_ctype arg_types)) (string_of_ctype ret_type)

(* Pretty printing a program - used by testing framework *)
let string_of_prog(p : prog) : string =
  let fundefs, body = p in
  String.concat "\n" ((List.map string_of_fundef fundefs) @ [string_of_expr body])
