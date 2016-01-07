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
    | CLASS  -> print_string "CLASS"
    | IDENTIFIER s -> print_string s
    | OPENING_BRACKET -> print_string "OPENING_BRACKET"
	| CLOSING_BRACKET -> print_string "CLOSING_BRACKET"
	| EXTENDS -> print_string "EXTENDS" 
	| IMPLEMENTS -> print_string "IMPLEMENTS"
	| COMMA -> print_string ";"
}

let identifierName = ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' '_' '$' 'A'-'Z']*
let space = [' ' '\t' '\n']
(* TODO COMMENTARIES*)
let commentLine = ['/']{2}[^(['\n' '\r'])]
rule nextToken = parse
  | eof {EOF}
  | "package" {PACKAGE}
  | "import" {IMPORT}
  | "static" {STATIC}
  | ";" {SEMICOLON}
  | "." {DOT}
  | "*" {STAR}
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
