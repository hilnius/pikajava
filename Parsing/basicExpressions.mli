exception Error

type token =
  | VAR of (string)
  | SUB
  | MUL
  | INT of (int)
  | EOF
  | DIV
  | ADD


val formule: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (operation)
