exception Error

type token = 
  | SEMICOLON
  | OPEN_PARENTHESIS
  | OPEN_BRACKET
  | INTEGER of (int)
  | IF
  | EOF
  | ELSE
  | CLOSE_PARENTHESIS
  | CLOSE_BRACKET
  | BOOLEAN of (bool)


val formule: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (block)