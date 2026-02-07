(* main.ml: エントリーポイント *)

open Ast

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.program Lexer.read lexbuf in

    Printf.printf ".intel_syntax noprefix\n";
    Printf.printf ".global main\n";

    (* グローバルデータ (文字列リテラル) の出力 *)
    if List.length prog.globals > 0 then begin
      Printf.printf ".data\n";
      List.iter (fun (id, str) ->
        Printf.printf ".LC%d:\n" id;
        (* 文字列のエスケープ処理などは簡易的に行う *)
        (* OCamlの文字列をそのまま出力すると問題がある場合があるので注意 *)
        (* ここでは .string ディレクティブを使う *)
        Printf.printf "  .string \"%s\"\n" (String.escaped str)
      ) prog.globals;
      Printf.printf ".text\n";
    end;

    (* 各関数のコード生成 *)
    List.iter (fun func ->
      Printf.printf "%s:\n" func.name;
      Printf.printf "  push rbp\n";
      Printf.printf "  mov rbp, rsp\n";
      let aligned_stack_size = (func.stack_size + 15) / 16 * 16 in
      if aligned_stack_size > 0 then
        Printf.printf "  sub rsp, %d\n" aligned_stack_size;

      let regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] in
      List.iteri (fun i _ ->
        if i < 6 then
          let reg = List.nth regs i in
          let offset = (i + 1) * 8 in
          Printf.printf "  mov [rbp-%d], %s\n" offset reg
        else
          failwith "More than 6 parameters not supported yet"
      ) func.args;

      List.iter (fun stmt ->
        Codegen.gen stmt;
        Printf.printf "  pop rax\n"
      ) func.body;

      Printf.printf "  mov rsp, rbp\n";
      Printf.printf "  pop rbp\n";
      Printf.printf "  ret\n"
    ) prog.funcs;

  with
  | Parsing.Parse_error ->
      Printf.eprintf "Parse error\n";
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
