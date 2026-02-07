{
  open Parser
}

let digit = ['0'-'9']
let space = [' ' '\t' '\n' '\r']

rule read = parse
  | space+ { read lexbuf }        (* 空白はスキップ *)
  | digit+ as n { INT (int_of_string n) } (* 整数リテラル *)
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
  | eof { EOF }                   (* ファイル終端 *)
  | _ { failwith "Unknown character" } (* 未知の文字はエラー *)
