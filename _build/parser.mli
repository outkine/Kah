
(* The type of tokens. *)

type token = 
  | TIMES_EQ
  | TIMES
  | TILDE
  | R_PAREN
  | R_BRACKET
  | R_BRACE
  | R_ARROW
  | PLUS_EQ
  | PLUS
  | OR
  | NOT_EQUALS
  | NOT
  | NEWLINE
  | MINUS_EQ
  | MINUS
  | MATCH
  | L_PAREN
  | L_BRACKET
  | L_BRACE
  | L_ARROW
  | INT of (int)
  | IF
  | ID of (string)
  | FUN
  | EQUALS
  | EQ
  | EOF
  | BOOL of (bool)
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr list)
