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
type expr = 
| Num of int64 
| Bool of bool
| Id of string
| Prim1 of prim1 * expr
| Prim2 of prim2 * expr * expr
| Let of string * expr * expr
| If of expr * expr * expr

(* Algebraic tagged datatype for expressions *)
type 'a eexpr =
| ENum of int64 * 'a
| EBool of bool * 'a
| EPrim1 of prim1 * 'a eexpr * 'a
| EPrim2 of prim2 * 'a eexpr * 'a eexpr * 'a
| EId of string * 'a
| ELet of string * 'a eexpr * 'a eexpr * 'a
| EIf of 'a eexpr * 'a eexpr * 'a eexpr * 'a

type tag = int

let tag_expr (e : expr) : tag eexpr =
  let rec help (e : expr) (cur : tag) : (tag eexpr * tag) =
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
      let (tag_e, next_tag) = help e (cur + 1) in
      (EPrim1 (op, tag_e, cur), next_tag)
    | Prim2 (op, e1, e2) ->
      let (tag_e1, next_tag1) = help e1 (cur + 1) in
      let (tag_e2, next_tag2) = help e2 next_tag1 in
      (EPrim2 (op, tag_e1, tag_e2, cur), next_tag2)
    | Let (x, e, b) ->
      let (tag_e, next_tag1) = help e (cur + 1) in
      let (tag_b, next_tag2) = help b next_tag1 in
      (ELet (x, tag_e, tag_b, cur), next_tag2)
    | If (c, t, e) ->
      let (tag_c, next_tag1) = help c (cur + 1) in
      let (tag_t, next_tag2) = help t next_tag1 in
      let (tag_e, next_tag3) = help e next_tag2 in
      (EIf (tag_c, tag_t, tag_e, cur), next_tag3)
  in let (tagged, _) = help e 1 in tagged;;

(* Pretty printing - used by testing framework *)
let rec string_of_expr(e : expr) : string = 
  match e with
  | Num n -> Int64.to_string n
  | Bool b -> Bool.to_string b
  | Id s -> s
  | Prim1 (op, e) -> sprintf "(%s %s)" 
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | Not -> "not") (string_of_expr e)
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

(* Pretty printing - used by testing framework *)
let rec string_of_eexpr(e : tag eexpr) : string = 
  match e with
  | ENum (n, _) -> Int64.to_string n
  | EBool (b, _) -> Bool.to_string b
  | EId (s, _) -> s
  | EPrim1 (op, e, _) -> sprintf "(%s %s)" 
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | Not -> "not") (string_of_eexpr e)
  | EPrim2 (op, e1, e2, _) -> sprintf "(%s %s %s)" 
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
  | ELet (x, e1, e2, _) -> sprintf "(let (%s %s) %s)" x (string_of_eexpr e1) (string_of_eexpr e2) 
  | EIf (e1, e2, e3, _) -> sprintf "(if %s %s %s)" (string_of_eexpr e1) (string_of_eexpr e2) (string_of_eexpr e3)
