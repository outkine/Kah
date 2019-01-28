type expr =
  (* values *)
  | Var of string
  | Array of expr list
  | Int of int
  | Bool of bool
  (* binary operators *)
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equals of expr * expr
  | NotEquals of expr * expr
  | Eq of string * expr
  | PlusEq of string * expr
  | MinusEq of string * expr
  | TimesEq of string * expr
