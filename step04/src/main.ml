(* main.ml: Step 2 *)
(* ASTを使って加減算をサポート *)

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.program Lexer.read lexbuf in

    Printf.printf ".intel_syntax noprefix\n";
    Printf.printf ".global main\n";
    Printf.printf "main:\n";

    (* スタックマシンのコード生成 *)
    Codegen.gen result;

    (* 結果はスタックトップにあるはずなので、raxにポップする *)
    Printf.printf "  pop rax\n";
    Printf.printf "  ret\n"
  with
  | Parsing.Parse_error ->
      Printf.eprintf "Parse error\n";
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
