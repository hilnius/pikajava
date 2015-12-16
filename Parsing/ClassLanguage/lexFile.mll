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
	| COMA -> print_string ";"
}

let identifierName = ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' '_' '$' 'A'-'Z']*
let space = [' ' '\t' '\n']
let className = ['A'-'Z']['0'-'9' 'a'-'z' '_' '$' 'A'-'Z']*
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
  | "extends" {EXTENDS}
  | "implements" {IMPLEMENTS}
  | "," {COMA}
  | "class" {CLASS}
  | identifierName as identifierName { IDENTIFIER identifierName }
  | '{' {OPENING_BRACKET}
  | '}' {CLOSING_BRACKET}
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
