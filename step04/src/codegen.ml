open Ast

(* スタックマシンのコード生成関数 *)
let rec gen = function
  | Int i ->
      Printf.printf "  push %d\n" i
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
  | Pos e ->
      (* 単項プラス: 何もしない *)
      gen e
  | Neg e ->
      (* 単項マイナス: 符号反転 *)
      gen e;
      Printf.printf "  pop rax\n";
      Printf.printf "  neg rax\n";
      Printf.printf "  push rax\n"
