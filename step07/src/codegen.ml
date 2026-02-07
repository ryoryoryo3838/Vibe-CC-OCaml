open Ast

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
