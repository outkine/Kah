{
open Core
open Parser
exception Error of string
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = (letter | '_' | digit)+
let bool = "True" | "False"
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white { read lexbuf }
  | newline+ { NEWLINE }
  | eof { EOF }
  (* brackets *)
  | "(" { L_PAREN }
  | ")" { R_PAREN }
  | "[" { L_BRACKET }
  | "]" { R_BRACKET }
  | "{" { L_BRACE }
  | "}" { R_BRACE }
  (* operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "and" { AND }
  | "or" { OR }
  | "==" { EQUAL }
  | "!=" { NOT_EQUAL }
  | "not" { NOT }
  (* assignments *)
  | "=" { EQ }
  | "+=" { PLUS_EQ }
  | "-=" { MINUS_EQ }
  | "*=" { TIMES_EQ }
  (* structures *)
  | "fun" { FUN }
  | "if" { IF }
  | "match" { MATCH }
  | "~" { TILDE }
  | "->" { R_ARROW }
  | "<-" { L_ARROW }
  (* values *)
  | bool { BOOL (Lexing.lexeme lexbuf |> String.lowercase |> Bool.of_string) }
  | int { INT (Lexing.lexeme lexbuf |> Int.of_string) }
  | id { ID (Lexing.lexeme lexbuf) }
  (* else *)
  | _ { raise (Error (Lexing.lexeme_start lexbuf |> Printf.sprintf "At offset %d: unexpected character.\n")) }

