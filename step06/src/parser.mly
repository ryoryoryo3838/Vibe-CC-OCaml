%{
  open Ast
%}

/* トークンの定義 */
%token <int> INT        /* 整数リテラル */
%token PLUS MINUS TIMES DIV /* 四則演算子 */
%token EQ NE LT LE GT GE    /* 比較演算子 */
%token LPAREN RPAREN    /* 括弧 */
%token EOF              /* ファイル終端 */

/* 演算子の優先順位と結合性 (下に行くほど優先順位が高い) */
%left EQ NE             /* 比較演算子 (等価) */
%left LT LE GT GE       /* 比較演算子 (大小) */
%left PLUS MINUS        /* 加減算 (左結合) */
%left TIMES DIV         /* 乗除算 (左結合) */
%nonassoc UMINUS        /* 単項マイナス (結合なし、高い優先順位) */

/* 開始記号 */
%start <Ast.t> program
%%

/* プログラム全体: 式が1つあり、その後にEOFが続く */
program:
  | e = expr; EOF { e }

/* 式の定義 */
expr:
  /* 比較演算 */
  | e1 = expr; EQ; e2 = expr { Eq (e1, e2) }
  | e1 = expr; NE; e2 = expr { Ne (e1, e2) }
  | e1 = expr; LT; e2 = expr { Lt (e1, e2) }
  | e1 = expr; LE; e2 = expr { Le (e1, e2) }
  | e1 = expr; GT; e2 = expr { Gt (e1, e2) }
  | e1 = expr; GE; e2 = expr { Ge (e1, e2) }

  /* 四則演算 */
  | e1 = expr; PLUS; e2 = expr { Add (e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Sub (e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Mul (e1, e2) }
  | e1 = expr; DIV; e2 = expr { Div (e1, e2) }

  /* 単項演算子 (優先順位を %prec で指定) */
  | MINUS; e = expr %prec UMINUS { Neg e }
  | PLUS; e = expr %prec UMINUS { Pos e }

  /* 括弧で囲まれた式 */
  | LPAREN; e = expr; RPAREN { e }

  /* 整数リテラル */
  | i = INT { Int i }
