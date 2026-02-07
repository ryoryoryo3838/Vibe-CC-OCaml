open Ast

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
