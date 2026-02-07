(* main.ml: エントリーポイント *)

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let (stmts, stack_size) = Parser.program Lexer.read lexbuf in

    Printf.printf ".intel_syntax noprefix\n";
    Printf.printf ".global main\n";
    Printf.printf "main:\n";

    (* プロローグ *)
    Printf.printf "  push rbp\n";
    Printf.printf "  mov rbp, rsp\n";

    (* ローカル変数の領域確保 *)
    (* スタックは16バイト境界に整列されている必要がある *)
    (* プロローグで push rbp しているので、この時点で RSP % 16 == 0 *)
    (* stack_size は 8 * N。*)
    (* sub rsp, stack_size すると、Nが奇数なら RSP % 16 == 8 になる。*)
    (* そこで、stack_size を 16 の倍数に切り上げる *)
    let aligned_stack_size = (stack_size + 15) / 16 * 16 in

    if aligned_stack_size > 0 then
      Printf.printf "  sub rsp, %d\n" aligned_stack_size;

    (* 各ステートメントのコード生成 *)
    List.iter (fun stmt ->
      Codegen.gen stmt;
      Printf.printf "  pop rax\n"
    ) stmts;

    (* エピローグ *)
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
