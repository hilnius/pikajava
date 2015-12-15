val examineAll : Lexing.lexbuf -> unit

val nextToken : Lexing.lexbuf -> ParseClass.token

val printLexeme : ParseClass.token-> string
