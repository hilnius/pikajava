{
open Parsers

let printLexeme = function
    | EOF     -> print_string "EOF"
    | PACKAGE  -> print_string "PACKAGE"
    | IMPORT  -> print_string "IMPORT"
    | STATIC  -> print_string "STATIC"
    | SEMICOLON -> print_string ";"
	| DOT -> print_string "."
	| STAR -> print_string "*"
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
    | OPENING_BRACKET -> print_string "OPENING_BRACKET"
	| CLOSING_BRACKET -> print_string "CLOSING_BRACKET"
	| OPENING_CHEVRON -> print_string "OPENING_CHEVRON"
  	| CLOSING_CHEVRON -> print_string "CLOSING_CHEVRON"
  	| WILDCARD -> print_string "WILDCARD"
  	| OPENING_BRACKET -> print_string "OPENING_BRACKET"
  	| CLOSING_BRACKET -> print_string "CLOSING_BRACKET"
  	| OPENING_PARENTHESIS -> print_string "OPENING_PARENTHESIS"
  	| CLOSING_PARENTHESIS -> print_string "CLOSING_PARENTHESIS"
  	| IF -> print_string "IF" 
  	| ELSE -> print_string "ELSE" 
  	| INTEGER(3) -> print_string "INTEGER(3)" 
  	| BOOLEAN(true) -> print_string "BOOLEAN(true)" 
  	| FOR -> print_string "FOR"
  	| WHILE -> print_string "WHILE"
  	| VOID -> print_string "VOID"
  	| IDENTIFIER s -> print_string s
}

let identifierName = ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' '_' '$' 'A'-'Z']*
let space = [' ' '\t']
let newLine = ['\n']
let iniStatic = "static"[' ' '\t' '\n']*"{"
(* TODO COMMENTARIES*)
(*let commentLine = ['/']{2}[^(['\n' '\r'])]*)

rule nextToken = parse
  | eof {EOF}
  | "package" {PACKAGE}
  | "import" {IMPORT}
  | iniStatic {INISTATIC}
  | "static" {STATIC}
  | ";" {SEMICOLON}
  | "." {DOT}
  | "*" {STAR}
  | newLine { Location.incr_line lexbuf; nextToken lexbuf }
  | space+  { nextToken lexbuf }
  | "public" {PUBLIC}
  | "protected" {PROTECTED}
  | "private" {PRIVATE}
  |	"abstract" {ABSTRACT}
  | "final" {FINAL}
  | "strictfp" {STRICTFP}
  | "native" {NATIVE}
  | "synchronized" {SYNCHRONIZED}
  | "extends" {EXTENDS}
  | "super" {SUPER}
  | "implements" {IMPLEMENTS}
  | "throws" {THROWS}
  | "," {COMMA}
  | "interface" {INTERFACE}
  | "class" {CLASS}
  | "enum" {ENUM}
  | "@" {AT}
  | "<" {OPENING_CHEVRON}
  | ">" {CLOSING_CHEVRON}
  | "?" {WILDCARD}
  | '{' {OPENING_BRACKET}
  | '}' {CLOSING_BRACKET}
  | '(' {OPENING_PARENTHESIS}
  | ')' {CLOSING_PARENTHESIS}
  | "if" { IF }
  | "else" { ELSE }
  | "int" { INTEGER(3) }
  | "true" { BOOLEAN(true) }
  | "for" { FOR }
  | "while" { WHILE }
  | "void" {VOID}
  | identifierName as identifierName { IDENTIFIER identifierName }

{
let rec examineAll lexbuf =
    let res = nextToken lexbuf in
    begin
		printLexeme res;
		print_string " ";
		match res with
		| EOF -> ()
		| _   -> examineAll lexbuf
    end;
    ();
}
