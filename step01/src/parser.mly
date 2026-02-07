%token <int> INT
%token EOF

%start <int> program
%%

program:
  | i = INT; EOF { i }
