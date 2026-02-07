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
%token IF ELSE WHILE FOR /* 制御構文 */
%token PLUS MINUS TIMES DIV /* 四則演算子 */
%token EQ NE LT LE GT GE    /* 比較演算子 */
%token ASSIGN           /* 代入演算子 */
%token LPAREN RPAREN    /* 括弧 */
%token LBRACE RBRACE    /* 中括弧 */
%token SEMI             /* セミコロン */
%token EOF              /* ファイル終端 */

/* 優先順位: if-else の shift/reduce conflict を解決するため */
/* ELSE の優先順位を高くする (shift優先) */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%start <Ast.program * int> program
%%

program:
  | stmts = list(stmt); EOF { (stmts, (List.length !locals) * 8) }

stmt:
  | e = expr; SEMI { e }
  | RETURN; e = expr; SEMI { Return e }
  | LBRACE; stmts = list(stmt); RBRACE { Block stmts }
  | IF; LPAREN; cond = expr; RPAREN; then_stmt = stmt; ELSE; else_stmt = stmt { If (cond, then_stmt, Some else_stmt) }
  | IF; LPAREN; cond = expr; RPAREN; then_stmt = stmt %prec LOWER_THAN_ELSE { If (cond, then_stmt, None) }
  | WHILE; LPAREN; cond = expr; RPAREN; body = stmt { While (cond, body) }
  | FOR; LPAREN; init = option(expr); SEMI; cond = option(expr); SEMI; inc = option(expr); RPAREN; body = stmt { For (init, cond, inc, body) }

expr:
  | e = assign { e }

assign:
  | name = IDENT; ASSIGN; e = assign { Assign (LVar (find_lvar name), e) }
  | e = equality { e }

equality:
  | e1 = equality; EQ; e2 = relational { Eq (e1, e2) }
  | e1 = equality; NE; e2 = relational { Ne (e1, e2) }
  | e = relational { e }

relational:
  | e1 = relational; LT; e2 = add { Lt (e1, e2) }
  | e1 = relational; LE; e2 = add { Le (e1, e2) }
  | e1 = relational; GT; e2 = add { Gt (e1, e2) }
  | e1 = relational; GE; e2 = add { Ge (e1, e2) }
  | e = add { e }

add:
  | e1 = add; PLUS; e2 = mul { Add (e1, e2) }
  | e1 = add; MINUS; e2 = mul { Sub (e1, e2) }
  | e = mul { e }

mul:
  | e1 = mul; TIMES; e2 = unary { Mul (e1, e2) }
  | e1 = mul; DIV; e2 = unary { Div (e1, e2) }
  | e = unary { e }

unary:
  | PLUS; e = unary { Pos e }
  | MINUS; e = unary { Neg e }
  | e = primary { e }

primary:
  | i = INT { Int i }
  | name = IDENT { LVar (find_lvar name) }
  | LPAREN; e = expr; RPAREN { e }
