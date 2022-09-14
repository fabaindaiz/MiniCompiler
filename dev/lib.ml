
open Ast
open Asm

type cond =
| CLt
| CGt
| CLte
| CGte
| CEq
| CNeq
  
let cond_expr (c : cond) (s : string) : (instruction list) =
  match c with
  | CLt -> [ IJl s ]
  | CGt -> [ IJg s ]
  | CLte -> [ IJle s ]
  | CGte -> [ IJge s ]
  | CEq -> [ IJe s ]
  | CNeq -> [ IJne s ]