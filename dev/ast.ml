(** AST **)
open Printf

(* primitivas unarias *)
type prim1 =
| Add1
| Sub1
| Not

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
type eexpr = 
| ENum of int64 
| EBool of bool
| EId of string
| EPrim1 of prim1 * eexpr
| EPrim2 of prim2 * eexpr * eexpr
| ELet of string * eexpr * eexpr
| EIf of eexpr * eexpr * eexpr

(* Algebraic tagged datatype for expressions *)
type 'a expr =
| Num of int64 * 'a
| Bool of bool * 'a
| Prim1 of prim1 * 'a expr * 'a
| Prim2 of prim2 * 'a expr * 'a expr * 'a
| Id of string * 'a
| Let of string * 'a expr * 'a expr * 'a
| If of 'a expr * 'a expr * 'a expr * 'a

type tag = int

let tag_expr (e : eexpr) : tag expr =
  let rec help (e : eexpr) (cur : tag) : (tag expr * tag) =
    match e with
    | ENum n ->
      let (tag_n, next_tag) = (cur, cur + 1) in
      (Num (n, tag_n), next_tag)
    | EBool b ->
      let (tag_b, next_tag) = (cur, cur + 1) in
      (Bool (b, tag_b), next_tag)
    | EId (x) ->
      let (tag_x, next_tag) = (cur, cur + 1) in
      (Id (x, tag_x), next_tag)
    | EPrim1 (op, e) ->
      let (tag_e, next_tag) = help e (cur + 1) in
      (Prim1 (op, tag_e, cur), next_tag)
    | EPrim2 (op, e1, e2) ->
      let (tag_e1, next_tag1) = help e1 (cur + 1) in
      let (tag_e2, next_tag2) = help e2 next_tag1 in
      (Prim2 (op, tag_e1, tag_e2, cur), next_tag2)
    | ELet (x, e, b) ->
      let (tag_e, next_tag1) = help e (cur + 1) in
      let (tag_b, next_tag2) = help b next_tag1 in
      (Let (x, tag_e, tag_b, cur), next_tag2)
    | EIf (c, t, e) ->
      let (tag_c, next_tag1) = help c (cur + 1) in
      let (tag_t, next_tag2) = help t next_tag1 in
      let (tag_e, next_tag3) = help e next_tag2 in
      (If (tag_c, tag_t, tag_e, cur), next_tag3)
  in let (tagged, _) = help e 1 in tagged;;

(* Pretty printing - used by testing framework *)
let rec string_of_eexpr(e : eexpr) : string = 
  match e with
  | ENum n -> Int64.to_string n
  | EBool b -> Bool.to_string b
  | EId s -> s
  | EPrim1 (op, e) -> sprintf "(%s %s)" 
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | Not -> "not") (string_of_eexpr e)
  | EPrim2 (op, e1, e2) -> sprintf "(%s %s %s)" 
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
    | Neq -> "!=") (string_of_eexpr e1) (string_of_eexpr e2)
  | ELet (x, e1, e2) -> sprintf "(let (%s %s) %s)" x (string_of_eexpr e1) (string_of_eexpr e2) 
  | EIf (e1, e2, e3) -> sprintf "(if %s %s %s)" (string_of_eexpr e1) (string_of_eexpr e2) (string_of_eexpr e3)

(* Pretty printing - used by testing framework *)
let rec string_of_expr(e : tag expr) : string = 
  match e with
  | Num (n, _) -> Int64.to_string n
  | Bool (b, _) -> Bool.to_string b
  | Id (s, _) -> s
  | Prim1 (op, e, _) -> sprintf "(%s %s)" 
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | Not -> "not") (string_of_expr e)
  | Prim2 (op, e1, e2, _) -> sprintf "(%s %s %s)" 
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
  | Let (x, e1, e2, _) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
  | If (e1, e2, e3, _) -> sprintf "(if %s %s %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
