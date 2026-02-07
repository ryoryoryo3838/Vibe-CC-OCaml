{
  open Parser
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let whitespace = [' ' '\t' '\r']

rule read = parse
  | whitespace+ { read lexbuf }        (* 空白はスキップ *)
  | '\n' { read lexbuf }          (* 改行もスキップ *)
  | "//" [^ '\n']* { read lexbuf } (* 行コメント: // から改行までスキップ (改行自体は次のマッチで処理) *)
  | "/*" { read_block_comment lexbuf }  (* ブロックコメント開始 *)
  | '#' [^ '\n']* { read lexbuf }  (* プリプロセッサ: # から改行までスキップ *)
  | digit+ as n { INT (int_of_string n) } (* 整数リテラル *)
  | "return" { RETURN }           (* return キーワード *)
  | "if" { IF }                   (* if キーワード *)
  | "else" { ELSE }               (* else キーワード *)
  | "while" { WHILE }             (* while キーワード *)
  | "for" { FOR }                 (* for キーワード *)
  | "==" { EQ }                   (* 比較演算子 *)
  | "!=" { NE }
  | "<=" { LE }
  | ">=" { GE }
  | '<' { LT }
  | '>' { GT }
  | '+' { PLUS }                  (* 四則演算子 *)
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }                (* 括弧 *)
  | ')' { RPAREN }
  | '{' { LBRACE }                (* 中括弧 *)
  | '}' { RBRACE }
  | '=' { ASSIGN }                (* 代入 *)
  | ';' { SEMI }                  (* セミコロン *)
  | ',' { COMMA }                 (* カンマ *)
  | lower alnum* as s { IDENT s } (* 識別子 *)
  | eof { EOF }                   (* ファイル終端 *)
  | _ { failwith (Printf.sprintf "Unknown character: %c" (Lexing.lexeme_char lexbuf 0)) }

and read_block_comment = parse
  | "*/" { read lexbuf }          (* コメント終了 *)
  | eof { failwith "Unterminated comment" }
  | _ { read_block_comment lexbuf } (* 中身はスキップ *)
