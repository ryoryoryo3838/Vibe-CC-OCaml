%{
  open Ast

  (* ローカル変数の管理用 *)
  let locals : (string * int) list ref = ref []

  (* 変数名からオフセットを取得する。存在しなければ新規登録 *)
  let find_lvar name =
    let rec find_in_list = function
      | [] -> None
      | (nm, off) :: rest -> if nm = name then Some off else find_in_list rest
    in
    match find_in_list !locals with
    | Some offset -> offset
    | None ->
        (* 9cc同様、新しい変数はスタックの深い位置に配置する *)
        let offset = (List.length !locals + 1) * 8 in
        locals := (name, offset) :: !locals;
        offset
%}

%token <int> INT        /* 整数リテラル */
%token <string> IDENT   /* 識別子 (変数名) */
%token RETURN           /* return */
%token PLUS MINUS TIMES DIV /* 四則演算子 */
%token EQ NE LT LE GT GE    /* 比較演算子 */
%token ASSIGN           /* 代入演算子 */
%token LPAREN RPAREN    /* 括弧 */
%token SEMI             /* セミコロン */
%token EOF              /* ファイル終端 */

/* 開始記号 */
/* AST(文のリスト)と、計算されたローカル変数の総サイズを返す */
%start <Ast.program * int> program
%%

/* プログラム全体: 複数の文がEOFまで続く */
/* 戻り値: (文のリスト, ローカル変数の総サイズ) */
program:
  | stmts = list(stmt); EOF { (stmts, (List.length !locals) * 8) }

/* 文 (statement) の定義 */
stmt:
  | e = expr; SEMI { e }            /* 式文: 式; */
  | RETURN; e = expr; SEMI { Return e } /* return文: return 式; */

/* 式 (expression) の定義 */
expr:
  | e = assign { e }

/* 代入: 右結合 */
assign:
  | name = IDENT; ASSIGN; e = assign { Assign (LVar (find_lvar name), e) }
  | e = equality { e }

/* 等価比較 (==, !=): 左結合 */
equality:
  | e1 = equality; EQ; e2 = relational { Eq (e1, e2) }
  | e1 = equality; NE; e2 = relational { Ne (e1, e2) }
  | e = relational { e }

/* 大小比較 (<, <=, >, >=): 左結合 */
relational:
  | e1 = relational; LT; e2 = add { Lt (e1, e2) }
  | e1 = relational; LE; e2 = add { Le (e1, e2) }
  | e1 = relational; GT; e2 = add { Gt (e1, e2) }
  | e1 = relational; GE; e2 = add { Ge (e1, e2) }
  | e = add { e }

/* 加減算 (+, -): 左結合 */
add:
  | e1 = add; PLUS; e2 = mul { Add (e1, e2) }
  | e1 = add; MINUS; e2 = mul { Sub (e1, e2) }
  | e = mul { e }

/* 乗除算 (*, /): 左結合 */
mul:
  | e1 = mul; TIMES; e2 = unary { Mul (e1, e2) }
  | e1 = mul; DIV; e2 = unary { Div (e1, e2) }
  | e = unary { e }

/* 単項演算子 (+, -) */
unary:
  | PLUS; e = unary { Pos e } (* +x -> 再帰的にunaryを呼ぶことで ++x などに対応 *)
  | MINUS; e = unary { Neg e } (* -x -> 再帰的にunaryを呼ぶことで --x などに対応 *)
  | e = primary { e }

/* 一次式 (リテラル, 変数, 括弧) */
primary:
  | i = INT { Int i }
  | name = IDENT { LVar (find_lvar name) }
  | LPAREN; e = expr; RPAREN { e }
