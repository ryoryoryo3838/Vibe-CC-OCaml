{
  open Parser
}

let digit = ['0'-'9']
let space = [' ' '\t' '\n' '\r']

rule read = parse
  | space+ { read lexbuf }
  | digit+ as n { INT (int_of_string n) }
  | eof { EOF }
  | _ { failwith "Unknown character" }
