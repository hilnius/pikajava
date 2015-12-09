{
}
let space = [' ' '\t' '\n']
rule nexttoken = parse
    space+ { nexttoken lexbuf }
  | eof { EOF }
  | '+' { ADD }
  | '-' { SUB }
  | '/' { DIV }
  | '*' { MUL }
  | ['0'-'9']+ as i { INTEGER (int_of_string i) }
  | ['a'-'z' 'A'-'Z'] ['_' '0'-'9' 'a'-'z' 'A'-'Z']* as v { IDENTIFIER v }
