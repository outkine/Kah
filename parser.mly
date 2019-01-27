%{
open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> ID

%token PLUS PLUS_EQ MINUS MINUS_EQ TIMES TIMES_EQ EQ
%token EQUALS NOT_EQUALS AND OR NOT
%token L_BRACKET R_BRACKET L_BRACE R_BRACE L_PAREN R_PAREN
%token L_ARROW R_ARROW
%token FUN MATCH IF TILDE
%token EOF NEWLINE

%left PLUS MINUS
%left TIMES
%left PLUS_EQ MINUS_EQ TIMES_EQ EQ
%nonassoc NOT
%left AND OR

%start <Ast.expr list> prog
%%

let prog :=
	es = separated_list(NEWLINE, expr); EOF; <>

let top :=
    | ~ = expr; <>
(*	| FUN; name = ID; ~ = body <FunDef>*)

let expr :=
	| ~ = INT; <Int>
	| ~ = BOOL; <Bool>
	| ~ = ID; <Var>
	| L_PAREN; ~ = expr; R_PAREN; <>

(*
	| e1 = expr; PLUS; e2 = expr <Plus>
	| e1 = expr; MINUS; e2 = expr { Minus(e1, e2) }
	| e1 = expr; TIMES; e2 = expr { Times(e1, e2) }
	| e1 = expr; AND; e2 = expr { And(e1, e2) }
	| e1 = expr; OR; e2 = expr { Or(e1, e2) }
	| e1 = expr; EQUALS; e2 = expr { Equals(e1, e2) }
	| e1 = expr; NOT_EQUALS; e2 = expr { NotEquals(e1, e2) }
	| NOT; e = expr { Not(e1, e2) }

	| name = ID; EQ; e = expr { Eq(name, e) }
	| name = ID; PLUS_EQ; e = expr { PlusEq(name, e) }
	| name = ID; MINUS_EQ; e = expr { MinusEq(name, e) }
	| name = ID; TIMES_EQ; e = expr { TimesEq(name, e) }

	| TILDE; name = ID { FunCall(name) }

	| MATCH; e = expr; b = match_body { Match(e, b) }
	;

match_body:
	| L_BRACE
    ;

body:
	| R_ARROW; e = expr { [e] }
	| L_BRACE; es = expressions; R_BRACE { es }
	;

expressions:
	| (* empty *) { [] }
	| e = expr { [e] }
	| e = expr; NEWLINE; es = expressions { e :: es }
	;
*)
