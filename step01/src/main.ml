(* main.ml: エントリーポイント *)
(* 標準入力からコードを読み込み、アセンブリを生成して標準出力に出力する *)

let () =
  try
    (* 標準入力を字句解析器に渡す *)
    let lexbuf = Lexing.from_channel stdin in

    (* 字句解析器と構文解析器を使ってプログラムをパースする *)
    (* Parser.program は parser.mly で定義された開始ルール *)
    let result = Parser.program Lexer.read lexbuf in

    (* アセンブリのプリアンブルを出力 *)
    Printf.printf ".intel_syntax noprefix\n"; (* Intel記法を使用 *)
    Printf.printf ".global main\n";           (* main関数をグローバルシンボルとして公開 *)
    Printf.printf "main:\n";

    (* 結果をraxレジスタに移動（raxは戻り値を格納するレジスタ） *)
    Printf.printf "  mov rax, %d\n" result;

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
