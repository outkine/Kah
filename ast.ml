type unary_operator = Not
type operator = Plus | Minus | Times | And | Or | Equal | NotEqual

type var = string

and body = expr list

and expr =
  (* structures *)
  | FunDef of var * body
  | FunCall of var
  | Match of expr * (expr * body) list
  | If of expr * body
  (* values *)
  | Var of var
  | Array of expr list
  | ArrayAccess of var * expr list
  | Int of int
  | Bool of bool
  (* operators *)
  | UnaryOp of unary_operator * expr
  | Op of operator * expr * expr
  | Eq of var * expr
  | OpEq of operator * var * expr
