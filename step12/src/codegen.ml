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
  | Str (id, _) ->
      (* 文字列リテラルのアドレスをロード *)
      Printf.printf "  lea rax, [rip + .LC%d]\n" id;
      Printf.printf "  push rax\n"
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
      List.iter gen args;
      let regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] in
      let num_args = List.length args in
      let pop_args n =
        if n > 0 then begin
          let used_regs = List.filteri (fun i _ -> i < min n 6) regs in
          List.iter (fun reg -> Printf.printf "  pop %s\n" reg) (List.rev used_regs);
          ()
        end
      in
      if num_args > 6 then failwith "More than 6 arguments not supported yet";
      pop_args num_args;
      let label_padding = new_label "padding" in
      let label_end = new_label "end" in
      Printf.printf "  mov rax, rsp\n";
      Printf.printf "  and rax, 15\n";
      Printf.printf "  jnz %s\n" label_padding;
      Printf.printf "  mov rax, 0\n";
      Printf.printf "  call %s\n" name;
      Printf.printf "  jmp %s\n" label_end;
      Printf.printf "%s:\n" label_padding;
      Printf.printf "  sub rsp, 8\n";
      Printf.printf "  mov rax, 0\n";
      Printf.printf "  call %s\n" name;
      Printf.printf "  add rsp, 8\n";
      Printf.printf "%s:\n" label_end;
      Printf.printf "  push rax\n"
  | If (cond, then_stmt, else_stmt_opt) ->
      let label_else = new_label "else" in
      let label_end = new_label "end" in
      gen cond;
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, 0\n";
      (match else_stmt_opt with
       | Some _ -> Printf.printf "  je %s\n" label_else
       | None -> Printf.printf "  je %s\n" label_end);
      gen then_stmt;
      Printf.printf "  pop rax\n";
      Printf.printf "  jmp %s\n" label_end;
      (match else_stmt_opt with
       | Some else_stmt ->
           Printf.printf "%s:\n" label_else;
           gen else_stmt;
           Printf.printf "  pop rax\n";
       | None -> ());
      Printf.printf "%s:\n" label_end;
      Printf.printf "  push 0\n"
  | While (cond, body) ->
      let label_begin = new_label "begin" in
      let label_end = new_label "end" in
      Printf.printf "%s:\n" label_begin;
      gen cond;
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, 0\n";
      Printf.printf "  je %s\n" label_end;
      gen body;
      Printf.printf "  pop rax\n";
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
      Printf.printf "  pop rax\n";
      (match inc with Some e -> gen e; Printf.printf "  pop rax\n" | None -> ());
      Printf.printf "  jmp %s\n" label_begin;
      Printf.printf "%s:\n" label_end;
      Printf.printf "  push 0\n"
  | Block stmts ->
      List.iter (fun stmt -> gen stmt; Printf.printf "  pop rax\n") stmts;
      Printf.printf "  push 0\n"
  | Add (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  add rax, rdi\n  push rax\n"
  | Sub (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  sub rax, rdi\n  push rax\n"
  | Mul (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  imul rax, rdi\n  push rax\n"
  | Div (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  cqo\n  idiv rdi\n  push rax\n"
  | Pos e -> gen e
  | Neg e -> gen e; Printf.printf "  pop rax\n  neg rax\n  push rax\n"
  | Eq (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  cmp rax, rdi\n  sete al\n  movzb rax, al\n  push rax\n"
  | Ne (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  cmp rax, rdi\n  setne al\n  movzb rax, al\n  push rax\n"
  | Lt (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  cmp rax, rdi\n  setl al\n  movzb rax, al\n  push rax\n"
  | Le (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  cmp rax, rdi\n  setle al\n  movzb rax, al\n  push rax\n"
  | Gt (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  cmp rax, rdi\n  setg al\n  movzb rax, al\n  push rax\n"
  | Ge (lhs, rhs) -> gen lhs; gen rhs; Printf.printf "  pop rdi\n  pop rax\n  cmp rax, rdi\n  setge al\n  movzb rax, al\n  push rax\n"
