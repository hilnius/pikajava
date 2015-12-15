{
}
let space = [' ' '\t' '\n']
rule nexttoken = parse
    space+ { nexttoken lexbuf }
  | eof { EOF }
  | '(' { OPEN_PARENTHESIS }
  | ')' { CLOSE_PARENTHESIS }
  | '{' { OPEN_BRACKET }
  | '}' { CLOSE_BRACKET }
  | "if" { IF }
  | "else" { ELSE }
  | "int" { INTEGER(3) }
  | "true" { BOOLEAN(true) }
  | "for" { FOR }
  | "while" { WHILE }
  | ',' { COMMA }
  | ';' { SEMICOLON }

{

}
