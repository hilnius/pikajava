
(* The type of tokens. *)

type token = 
  | WHILE
  | SEMICOLON
  | OPEN_PARENTHESIS
  | OPEN_BRACKET
  | INTEGER of (int)
  | IF
  | FOR
  | EOF
  | ELSE
  | COMMA
  | CLOSE_PARENTHESIS
  | CLOSE_BRACKET
  | BOOLEAN of (bool)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val formule: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (block)
