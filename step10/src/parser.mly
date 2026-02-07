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
        let offset =
          match !locals with
          | [] -> 8
          | (_, off) :: _ -> off + 8
        in
        locals := (name, offset) :: !locals;
        offset

  (* パラメータをローカル変数として登録する *)
  let register_params params =
    locals := []; (* 新しい関数のためにリセット *)
    List.iter (fun name ->
      let _ = find_lvar name in
      ()
    ) params
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
%token COMMA            /* カンマ */
%token EOF              /* ファイル終端 */

/* 優先順位: if-else の shift/reduce conflict を解決するため */
/* ELSE の優先順位を高くする (shift優先) */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%start <Ast.program> program
%%

program:
  | funcs = list(toplevel); EOF { funcs }

/* 関数定義 */
/* Menhirの制限（あるいはバグ？）により、複雑なアクションや mid-rule action の構文が厳密である可能性がある。
   一旦、mid-rule action を使わず、パラメータ登録を後回しにするか、
   あるいは、パラメータ付きルール（parameterized rule）を使ってみる。

   しかし、parser.mly 内で副作用を行うのが手っ取り早い。

   Menhirのエラーメッセージ:
   Error: syntax error after 'stmts' and before '='.

   これは、アクション内のOCaml構文エラーではなく、Menhirの構文エラー。

   start_stmts = mid_rule_init
   stmts = list(stmt); RBRACE
   { ... }

   この書き方は正しいはずだが...

   試行錯誤:
   register_params params を呼ぶためのダミー非終端記号を作る。
   この非終端記号は params を引数に取る必要があるが、Menhirの標準機能では難しい。

   妥協案:
   toplevelルール全体のアクションで register_params を呼び出すことはできない（stmtsのパースが終わっているため）。

   ここは、Menhirの推奨されるパターンの1つである、
   func_decl: name LPAREN params RPAREN LBRACE { register_params params; (name, params) }
   toplevel: decl = func_decl; stmts = list(stmt); RBRACE { ... }

   と分割することで、LBRACEの直後にアクションを実行できる。
*/

toplevel:
  | decl = func_decl; stmts = list(stmt); RBRACE
    {
      let (name, params) = decl in
      let stack_size =
        match !locals with
        | [] -> 0
        | (_, off) :: _ -> off
      in
      { Ast.name = name; args = params; body = stmts; stack_size = stack_size }
    }

func_decl:
  | name = IDENT; LPAREN; params = separated_list(COMMA, IDENT); RPAREN; LBRACE
    {
      register_params params;
      (name, params)
    }

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
  | name = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN { Call (name, args) } (* 関数呼び出し *)
  | name = IDENT { LVar (find_lvar name) }
  | LPAREN; e = expr; RPAREN { e }
