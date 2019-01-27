(*
open Core
open Ast

*)
(*
(* [subst e1 e2 x] is [e1] with [e2] substituted for [x]. *)
let rec subst e1 e2 x =
  match e1 with
  | Var y -> if x = y then e2 else e1
  | Int c -> Int c
  | Add (el, er) -> Add (subst el e2 x, subst er e2 x)
  | Let (y, ebind, ebody) ->
      if x = y then Let (y, subst ebind e2 x, ebody)
      else Let (y, subst ebind e2 x, subst ebody e2 x)

(* A single step of evaluation. *)
let rec step = function
  | Int _ -> failwith "Does not step"
  | Var _ -> failwith "Unbound variable"
  | Add (Int n1, Int n2) -> Int (n1 + n2)
  | Add (Int n1, e2) -> Add (Int n1, step e2)
  | Add (e1, e2) -> Add (step e1, e2)
  | Let (x, Int n, e2) -> subst e2 (Int n) x
  | Let (x, e1, e2) -> Let (x, step e1, e2)

*)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
