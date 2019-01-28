type unary_operator = Not
type operator = Plus | Minus | Times | And | Or | Equal | NotEqual

type id = string

and body = expr list

and array_info = expr list

and expr =
  (* structures *)
  | FunDef of id * body
  | FunCall of id
  | Match of expr * (expr * body) list
  | If of expr * body
  (* values *)
  | Var of var
  | Array of array_info
  | Int of int
  | Bool of bool
  (* operators *)
  | UnaryOp of unary_operator * expr
  | Op of operator * expr * expr
  | Eq of var * expr
  | OpEq of operator * var * expr
  | MemWrite of expr * expr

and var = RegVar of id | ArrayVar of id * array_info
