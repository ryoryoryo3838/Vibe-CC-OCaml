(* main.ml: エントリーポイント *)
(* 標準入力からコードを読み込み、パーサーでASTに変換し、コード生成器でアセンブリを出力する *)

let () =
  try
    (* 標準入力を字句解析器に渡す *)
    let lexbuf = Lexing.from_channel stdin in

    (* 字句解析器と構文解析器を使ってプログラムをパースする *)
    let result = Parser.program Lexer.read lexbuf in

    (* アセンブリのプリアンブルを出力 *)
    Printf.printf ".intel_syntax noprefix\n"; (* Intel記法を使用 *)
    Printf.printf ".global main\n";           (* main関数をグローバルシンボルとして公開 *)
    Printf.printf "main:\n";

    (* スタックマシンのコード生成 *)
    (* 式の評価結果はスタックトップに残る *)
    Codegen.gen result;

    (* 結果をraxレジスタに移動（raxは戻り値を格納するレジスタ） *)
    Printf.printf "  pop rax\n";

    (* 関数から戻る *)
    Printf.printf "  ret\n"
  with
  | Parsing.Parse_error ->
      (* パースエラー時の処理 *)
      Printf.eprintf "Parse error\n";
      exit 1
  | Failure msg ->
      (* その他のエラー時の処理 *)
      Printf.eprintf "Error: %s\n" msg;
      exit 1
