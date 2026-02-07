(* main.ml: エントリーポイント *)

let () =
  try
    let lexbuf = Lexing.from_channel stdin in

    (* programルールは (stmt list, local_vars_size) を返す *)
    let (stmts, stack_size) = Parser.program Lexer.read lexbuf in

    Printf.printf ".intel_syntax noprefix\n";
    Printf.printf ".global main\n";
    Printf.printf "main:\n";

    (* プロローグ *)
    Printf.printf "  push rbp\n";
    Printf.printf "  mov rbp, rsp\n";
    (* ローカル変数の領域確保 *)
    Printf.printf "  sub rsp, %d\n" stack_size;

    (* 各ステートメントのコード生成 *)
    List.iter (fun stmt ->
      Codegen.gen stmt;
      (* 式の評価結果としてスタックに値が積まれているはずなので、それをポップする *)
      Printf.printf "  pop rax\n"
    ) stmts;

    (* エピローグ *)
    (* 最後の式の結果がraxに入っている状態でここに来る *)
    Printf.printf "  mov rsp, rbp\n";
    Printf.printf "  pop rbp\n";
    Printf.printf "  ret\n"
  with
  | Parsing.Parse_error ->
      Printf.eprintf "Parse error\n";
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
