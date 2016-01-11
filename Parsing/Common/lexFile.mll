{
open Parsers

let printLexeme = function
    | EOF     -> print_string "EOF"
    | PACKAGE  -> print_string "PACKAGE"
    | IMPORT  -> print_string "IMPORT"
    | STATIC  -> print_string "STATIC"
    | SEMICOLON -> print_string ";"
	| DOT -> print_string "."
	| ASTERISK -> print_string "*"
(* Class Declaration*)
	| PUBLIC  -> print_string "PUBLIC"
    | PRIVATE  -> print_string "PRIVATE"
    | PROTECTED  -> print_string "PROTECTED"
    | ABSTRACT -> print_string "ABSTRACT"
    | FINAL -> print_string "FINAL"
    | STRICTFP -> print_string "STRICTFP"
    | NATIVE -> print_string "SYNCHRONIZED"
	| EXTENDS -> print_string "EXTENDS"
    | SUPER -> print_string "SUPER"
	| IMPLEMENTS -> print_string "IMPLEMENTS"
	| THROWS -> print_string "THROWS"
	| COMMA -> print_string ";"
	| INTERFACE -> print_string "INTERFACE"
    | CLASS  -> print_string "CLASS"
    | ENUM -> print_string "ENUM"
    | AT -> print_string "AT"
    | OPENING_BRACE -> print_string "OPENING_BRACE"
	| CLOSING_BRACE -> print_string "CLOSING_BRACE"
	| OPENING_CHEVRON -> print_string "OPENING_CHEVRON"
  	| CLOSING_CHEVRON -> print_string "CLOSING_CHEVRON"
  	| QUESTION_MARK -> print_string "QUESTION_MARK"
  	| OPENING_BRACE -> print_string "OPENING_BRACE"
  	| CLOSING_BRACE -> print_string "CLOSING_BRACE"
  	| OPENING_PARENTHESIS -> print_string "OPENING_PARENTHESIS"
  	| CLOSING_PARENTHESIS -> print_string "CLOSING_PARENTHESIS"
  	| IF -> print_string "IF"
  	| ELSE -> print_string "ELSE"
  	| FOR -> print_string "FOR"
  	| WHILE -> print_string "WHILE"
  	| VOID -> print_string "VOID"
  	| IDENTIFIER s -> print_string s

exception SyntaxError of string

}

let identifierName = ['a'-'z' 'A'-'Z'] ['_' '$' '0'-'9' 'a'-'z' 'A'-'Z']*
let float = ['0'-'9']+ ['.'] ['0'-'9']*
let integer = ['0'-'9']+
let space = [' ' '\t']
let newLine = ['\n']
(* TODO COMMENTARIES*)
(*let commentLine = ['/']{2}[^(['\n' '\r'])]*)

rule nextToken = parse
  | eof           { EOF }
  | newLine       { Location.incr_line lexbuf; nextToken lexbuf }
  | space+        { nextToken lexbuf }

  | "//"          { commentLine lexbuf }
  | "/*"          { longComment lexbuf }
  | '"'           { readString (Buffer.create 32) lexbuf }

  | ';'           { SEMICOLON }
  | ':'           { COLON }
  | '.'           { DOT }
  | ','           { COMMA }
  | '*'           { ASTERISK }
  | '?'           { QUESTION_MARK }
  | '+'           { MORE }
  | '-'           { LESS }
  | '!'           { EXCLAMATION_MARK }
  | '~'           { TILD }
  | '*'           { ASTERISK }
  | '/'           { DIVIDE }
  | '%'           { MODULO }
  | '@'           { AT }
  | '<'           { OPENING_CHEVRON }
  | '>'           { CLOSING_CHEVRON }
  | '.'           { DOT }
  | ','           { COMMA }
  | '|'           { BITOR }
  | '^'           { BITXOR }
  | '&'           { BITAND }

  | '('           { OPENING_PARENTHESIS }
  | ')'           { CLOSING_PARENTHESIS }
  | '['           { BRACKETOPEN }
  | ']'           { BRACKETCLOSE }
  | '{'           { OPENING_BRACE }
  | '}'           { CLOSING_BRACE }

  | "..."         { VARIADIC }
  | '='           { EQUAL }
  | "++"          { MOREMORE }
  | "--"          { LESSLESS }
  | "||"          { OR }
  | "&&"          { AND }
  | "=="          { EQUALEQUAL }
  | "!="          { DIFFERENT }
  | "<<"          { SHIFTLEFT }
  | ">>"          { SHIFTRIGHT }
  | ">>>"         { SHIFT_RIGHT_UNSIGNED }
  | "<="          { LESSEQUALTHAN }
  | ">="          { GREATEREQUALTHAN }
  | "+="          { MORE_EQUAL }
  | "-="          { LESS_EQUAL }
  | "*="          { ASTERISK_EQUAL }
  | "/="          { DIVIDE_EQUAL }
  | "&="          { AND_EQUAL }
  | "|="          { OR_EQUAL }
  | "^="          { XOR_EQUAL }
  | "%="          { MODULO_EQUAL }
  | "<<="         { LEFT_EQUAL }
  | ">>="         { RIGHT_EQUAL }
  | ">>>="        { SHIFT_RIGHT_UNSIGNED_EQUAL }

  | "package"     { PACKAGE }
  | "import"      { IMPORT }
  | "static"      { STATIC }
  | "public"      { PUBLIC }
  | "protected"   { PROTECTED }
  | "private"     { PRIVATE }
  |	"abstract"    { ABSTRACT }
  | "final"       { FINAL }
  | "strictfp"    { STRICTFP }
  | "native"      { NATIVE }
  | "synchronized"{ SYNCHRONIZED }
  | "extends"     { EXTENDS }
  | "super"       { SUPER }
  | "implements"  { IMPLEMENTS }
  | "throws"      { THROWS }
  | "interface"   { INTERFACE }
  | "class"       { CLASS }
  | "enum"        { ENUM }
  | "if"          { IF }
  | "else"        { ELSE }
  | "for"         { FOR }
  | "while"       { WHILE }
  | "assert"      { ASSERT }
  | "break"       { BREAK }
  | "continue"    { CONTINUE }
  | "switch"      { SWITCH }
  | "case"        { CASE }
  | "default"     { DEFAULT }
  | "do"          { DO }
  | "return"      { RETURN }
  | "throw"       { THROW }
  | "try"         { TRY }
  | "catch"       { CATCH }
  | "finally"     { FINALLY }

  | "new"         { NEW }
  | "void"        { VOID }
  | "this"        { THIS }
  | "instanceof"  { INSTANCEOF }

  | "byte"        { BYTE }
  | "short"       { SHORTINT }
  | "char"        { CHAR }
  | "int"         { INTEGER }
  | "long"        { LONGINT }
  | "float"       { FLOAT }
  | "double"      { DOUBLE }
  | "boolean"     { BOOLEAN }

  | integer as i  { print_string "integer numeral : "; print_string i; INTEGER_NUMERAL (int_of_string i) }
  | float as i    { print_string "floating point numeral : "; print_string i; FLOATING_POINT_NUMERAL (float_of_string i) }
  | "true"        { BOOLEAN_LITERAL true }
  | "false"       { BOOLEAN_LITERAL false }
  | "null"        { NULL }

  | identifierName as v { print_string ("identifier : " ^ v ^ "\n"); IDENTIFIER v }

and commentLine = parse
  | newLine       { Location.incr_line lexbuf; nextToken lexbuf }
  | _             { commentLine lexbuf }
and longComment = parse
  | "*/"          { nextToken lexbuf }
  | newLine       { Location.incr_line lexbuf; longComment lexbuf }
  | _             { longComment lexbuf }
and readString buf = parse
  | '"'           { STRING_LITERAL (Buffer.contents buf) }
  | '\\' '/'      { Buffer.add_char buf '/'; readString buf lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; readString buf lexbuf }
  | '\\' 'b'      { Buffer.add_char buf '\b'; readString buf lexbuf }
  | '\\' 'f'      { Buffer.add_char buf '\012'; readString buf lexbuf }
  | '\\' 'n'      { Buffer.add_char buf '\n'; readString buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; readString buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; readString buf lexbuf }
  | '\\' '"'      { Buffer.add_char buf '\"'; readString buf lexbuf }
  | '\\' '\''     { Buffer.add_string buf "\'"; readString buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      readString buf lexbuf
    }
  | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("String is not terminated")) }
{
}
