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
  | Call (name, args) ->
      (* 引数を評価してスタックに積む *)
      List.iter gen args;

      (* 引数をレジスタにポップする (逆順に積まれているので注意) *)
      (* System V AMD64 ABI: RDI, RSI, RDX, RCX, R8, R9 *)
      let regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] in
      let num_args = List.length args in
      let pop_args n =
        if n > 0 then begin
          let used_regs = List.filteri (fun i _ -> i < min n 6) regs in
          List.iter (fun reg ->
            Printf.printf "  pop %s\n" reg
          ) (List.rev used_regs);
          ()
        end
      in

      (* 簡易実装: 引数は6個までとする *)
      if num_args > 6 then failwith "More than 6 arguments not supported yet";

      pop_args num_args;

      (* call命令の呼び出し *)
      (* 可変長引数関数のために AL=0 をセット *)
      Printf.printf "  mov rax, 0\n";
      Printf.printf "  call %s\n" name;
      Printf.printf "  push rax\n"

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
