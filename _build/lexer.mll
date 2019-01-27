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
  | "\n"  { NEWLINE }
  | "+"   { PLUS }
  | "="   { EQUALS }
  | id    { ID (Lexing.lexeme lexbuf) }
  | int   { INT (Lexing.lexeme lexbuf |> Int.of_string) }
  | bool  { BOOL (Lexing.lexeme lexbuf |> String.lowercase |> Bool.of_string) }
  | eof   { EOF }
  | _
	  { raise (Error (Lexing.lexeme_start lexbuf |> Printf.sprintf "At offset %d: unexpected character.\n")) }

