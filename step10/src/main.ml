(* main.ml: エントリーポイント *)

open Ast

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let funcs = Parser.program Lexer.read lexbuf in

    Printf.printf ".intel_syntax noprefix\n";
    Printf.printf ".global main\n";

    (* 各関数のコード生成 *)
    List.iter (fun func ->
      Printf.printf "%s:\n" func.name;

      (* プロローグ *)
      Printf.printf "  push rbp\n";
      Printf.printf "  mov rbp, rsp\n";

      (* スタック領域確保 *)
      let aligned_stack_size = (func.stack_size + 15) / 16 * 16 in
      if aligned_stack_size > 0 then
        Printf.printf "  sub rsp, %d\n" aligned_stack_size;

      (* パラメータをローカル変数領域にコピー *)
      (* ABI: RDI, RSI, RDX, RCX, R8, R9 *)
      let regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] in
      List.iteri (fun i _ ->
        if i < 6 then
          let reg = List.nth regs i in
          (* パラメータのオフセットは、register_params で登録した順序、つまり先頭から offset 8, 16... となっているはず *)
          let offset = (i + 1) * 8 in
          Printf.printf "  mov [rbp-%d], %s\n" offset reg
        else
          failwith "More than 6 parameters not supported yet"
      ) func.args;

      (* 関数の本体のコード生成 *)
      List.iter (fun stmt ->
        Codegen.gen stmt;
        Printf.printf "  pop rax\n"
      ) func.body;

      (* エピローグ *)
      Printf.printf "  mov rsp, rbp\n";
      Printf.printf "  pop rbp\n";
      Printf.printf "  ret\n"
    ) funcs;

  with
  | Parsing.Parse_error ->
      Printf.eprintf "Parse error\n";
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
