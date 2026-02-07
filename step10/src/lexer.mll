{
  open Parser
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let space = [' ' '\t' '\n' '\r']

rule read = parse
  | space+ { read lexbuf }        (* 空白はスキップ *)
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
  | _ { failwith "Unknown character" } (* 未知の文字はエラー *)
