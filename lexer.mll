{
open Core
open Parser
exception Error of string
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let bool = "True" | "False"

rule read =
  parse
  | white { read lexbuf }
  | "\n"+ { NEWLINE }
  | "\n"* eof { EOF }
  | "(" { L_PAREN }
  | ")" { R_PAREN }
  | "[" { L_BRACE }
  | "]" { R_BRACE }
  (* binary operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "and" { AND }
  | "or" { OR }
  | "==" { EQUALS }
  | "!=" { NOT_EQUALS }
  | "not" { NOT }
  (* assignments *)
  | "==" { EQ }
  | "+=" { PLUS_EQ }
  | "-=" { MINUS_EQ }
  | "*=" { TIMES_EQ }
  | "==" { EQUALS }
  | "!=" { NOT_EQUALS }
  | "not" { NOT }
  (* values *)
  | bool { BOOL (Lexing.lexeme lexbuf |> String.lowercase |> Bool.of_string) }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (Lexing.lexeme lexbuf |> Int.of_string) }
  (* else *)
  | _ { raise (Error (Lexing.lexeme_start lexbuf |> Printf.sprintf "At offset %d: unexpected character.\n")) }

