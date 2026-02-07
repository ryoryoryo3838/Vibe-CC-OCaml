%{
  open Ast
%}

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIV

%start <Ast.t> program
%%

program:
  | e = expr; EOF { e }

expr:
  | e1 = expr; PLUS; e2 = expr { Add (e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Sub (e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Mul (e1, e2) }
  | e1 = expr; DIV; e2 = expr { Div (e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  | i = INT { Int i }
