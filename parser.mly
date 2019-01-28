%{
open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> ID

%token PLUS PLUS_EQ MINUS MINUS_EQ TIMES TIMES_EQ EQ
%token EQUAL NOT_EQUAL AND OR NOT
%token L_BRACKET R_BRACKET L_BRACE R_BRACE L_PAREN R_PAREN
%token L_ARROW R_ARROW
%token FUN MATCH IF TILDE
%token EOF NEWLINE

%left AND OR
%nonassoc NOT
%left PLUS MINUS
%left TIMES

%start <Ast.expr list> prog
%%

let prog :=
    es = newlines(top); EOF; <>

let top :=
    | ~ = expr; <>
    | FUN; name = ID; b = body; <FunDef>

let expr :=
    | ~ = INT; <Int>
    | ~ = BOOL; <Bool>
    | L_PAREN; e = expr; R_PAREN; <>
    | es = array+; <Array>

    | e1 = expr; op = op; e2 = expr; {Op(op, e1, e2)}
    | op = unary_op; e = expr; <UnaryOp>

    | ~ = var; <Var>
    | var = var; EQ; e = expr; <Eq>
    | var = var; op = eq_op; e = expr; {OpEq(op, var, e)}

    | TILDE; name = ID; <FunCall>
    | MATCH; e = expr; b = match_body; <Match>
    | IF; e = expr; b = body; <If>

let var :=
    | name = ID; <RegVar>
    | name = ID; es = array+; <ArrayVar>

let eq_op ==
    | PLUS_EQ; {Plus}
    | MINUS_EQ; {Minus}
    | TIMES_EQ; {Times}

let unary_op ==
    | NOT; {Not}

let op ==
    | PLUS; {Plus}
    | MINUS; {Minus}
    | TIMES; {Times}
    | AND; {And}
    | OR; {Or}
    | EQUAL; {Equal}
    | NOT_EQUAL; {NotEqual}

let array :=
    | L_BRACKET; e = expr; R_BRACKET; <>

let match_body :=
    | L_BRACE; es = newlines(match_field); R_BRACE; <>

let match_field :=
    | e = expr; b = body; <>

let body :=
    | R_ARROW; e = expr; {[e]}
    | L_BRACE; es = newlines(expr); R_BRACE; <>

let newlines(X) :=
    | NEWLINE?; { [] }
    | NEWLINE?; x = X; NEWLINE?; {[x]}
    | NEWLINE?; x = X; NEWLINE; xs = newlines(X); {x :: xs}
