{
open ParseClass

let printLexeme = function
    | EOF     -> print_string "EOF"
    | PUBLIC  -> print_string "PUBLIC"
    | PRIVATE  -> print_string "PRIVATE"
    | PROTECTED  -> print_string "PROTECTED"
    | CLASS  -> print_string "CLASS"
    | CLASS_NAME s -> print_string s
    | OPENING_BRACKET -> print_string "OPENING_BRACKET"
		| CLOSING_BRACKET -> print_string "CLOSING_BRACKET" 		
		
}

let className = ['A'-'Z']['0'-'9' 'a'-'z' '_' '$' 'A'-'Z']*

let space = [' ' '\t' '\n']

rule nextToken = parse
  | eof {EOF}
  | "public" {PUBLIC}
  | "protected" {PROTECTED}
  | "private" {PRIVATE}
  |	"abstract" {ABSTRACT}
  | "final" {FINAL}
  | space+  { nextToken lexbuf }
  | "class" {CLASS}
  | className as className    { CLASS_NAME className }
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
