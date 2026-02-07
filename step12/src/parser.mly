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

  (* 文字列リテラルの管理用 *)
  let strings : (int * string) list ref = ref []
  let string_counter = ref 0
  let new_string s =
    let id = !string_counter in
    incr string_counter;
    strings := (id, s) :: !strings;
    id
%}

%token <int> INT        /* 整数リテラル */
%token <string> STRING  /* 文字列リテラル */
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

/* 優先順位 */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%start <Ast.program> program
%%

program:
  | funcs = list(toplevel); EOF
    {
      { funcs = funcs; globals = List.rev !strings }
    }

/* 関数定義 */
/* アクション内で副作用を起こすために、非終端記号 func_decl を定義し、
   その還元時のアクションで params を登録する。
   list(stmt) のパース開始時には params が locals に登録されている状態になる。
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
  | s = STRING { Str (new_string s, s) } (* 文字列リテラル *)
  | name = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN { Call (name, args) }
  | name = IDENT { LVar (find_lvar name) }
  | LPAREN; e = expr; RPAREN { e }
