open Ast

(* ラベルの生成用 *)
let label_counter = ref 0
let new_label prefix =
  let count = !label_counter in
  incr label_counter;
  Printf.sprintf ".L%s%d" prefix count

(* 左辺値のアドレスを生成する関数 *)
let gen_lval = function
  | LVar offset ->
      Printf.printf "  mov rax, rbp\n";
      Printf.printf "  sub rax, %d\n" offset;
      Printf.printf "  push rax\n"
  | _ -> failwith "Not a left value"

(* スタックマシンのコード生成関数 *)
let rec gen = function
  | Int i ->
      Printf.printf "  push %d\n" i
  | LVar offset ->
      gen_lval (LVar offset);
      Printf.printf "  pop rax\n";
      Printf.printf "  mov rax, [rax]\n";
      Printf.printf "  push rax\n"
  | Assign (lhs, rhs) ->
      gen_lval lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  mov [rax], rdi\n";
      Printf.printf "  push rdi\n"
  | Return e ->
      gen e;
      Printf.printf "  pop rax\n";
      Printf.printf "  mov rsp, rbp\n";
      Printf.printf "  pop rbp\n";
      Printf.printf "  ret\n"
  | If (cond, then_stmt, else_stmt_opt) ->
      let label_else = new_label "else" in
      let label_end = new_label "end" in
      gen cond;
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, 0\n";
      (* 条件が偽なら else 節または end へジャンプ *)
      (match else_stmt_opt with
       | Some _ -> Printf.printf "  je %s\n" label_else
       | None -> Printf.printf "  je %s\n" label_end);
      gen then_stmt;
      Printf.printf "  pop rax\n"; (* then_stmtの結果を捨てる *)
      Printf.printf "  jmp %s\n" label_end;
      (match else_stmt_opt with
       | Some else_stmt ->
           Printf.printf "%s:\n" label_else;
           gen else_stmt;
           Printf.printf "  pop rax\n"; (* else_stmtの結果を捨てる *)
       | None -> ());
      Printf.printf "%s:\n" label_end;
      Printf.printf "  push 0\n" (* If文全体の結果としてダミーをプッシュ *)

  | While (cond, body) ->
      let label_begin = new_label "begin" in
      let label_end = new_label "end" in
      Printf.printf "%s:\n" label_begin;
      gen cond;
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, 0\n";
      Printf.printf "  je %s\n" label_end;
      gen body;
      Printf.printf "  pop rax\n"; (* bodyの評価結果を捨てる *)
      Printf.printf "  jmp %s\n" label_begin;
      Printf.printf "%s:\n" label_end;
      Printf.printf "  push 0\n"

  | For (init, cond, inc, body) ->
      let label_begin = new_label "begin" in
      let label_end = new_label "end" in
      (match init with Some e -> gen e; Printf.printf "  pop rax\n" | None -> ());
      Printf.printf "%s:\n" label_begin;
      (match cond with
       | Some e ->
           gen e;
           Printf.printf "  pop rax\n";
           Printf.printf "  cmp rax, 0\n";
           Printf.printf "  je %s\n" label_end
       | None -> ());
      gen body;
      Printf.printf "  pop rax\n"; (* bodyの評価結果を捨てる *)
      (match inc with Some e -> gen e; Printf.printf "  pop rax\n" | None -> ());
      Printf.printf "  jmp %s\n" label_begin;
      Printf.printf "%s:\n" label_end;
      Printf.printf "  push 0\n"

  | Block stmts ->
      List.iter (fun stmt ->
        gen stmt;
        Printf.printf "  pop rax\n" (* 各ステートメントの結果を捨てる *)
      ) stmts;
      Printf.printf "  push 0\n" (* ブロック全体の値（とりあえず0） *)

  | Add (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  add rax, rdi\n";
      Printf.printf "  push rax\n"
  | Sub (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  sub rax, rdi\n";
      Printf.printf "  push rax\n"
  | Mul (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  imul rax, rdi\n";
      Printf.printf "  push rax\n"
  | Div (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cqo\n";
      Printf.printf "  idiv rdi\n";
      Printf.printf "  push rax\n"
  | Pos e -> gen e
  | Neg e ->
      gen e;
      Printf.printf "  pop rax\n";
      Printf.printf "  neg rax\n";
      Printf.printf "  push rax\n"
  | Eq (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  sete al\n";
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Ne (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setne al\n";
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Lt (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setl al\n";
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Le (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setle al\n";
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Gt (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setg al\n";
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Ge (lhs, rhs) ->
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setge al\n";
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
