%{
  open Ast
%}

%token <int> INT
%token PLUS MINUS
%token EOF

%left PLUS MINUS

%start <Ast.t> program
%%

program:
  | e = expr; EOF { e }

expr:
  | i = INT { Int i }
  | e1 = expr; PLUS; e2 = expr { Add (e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Sub (e1, e2) }
