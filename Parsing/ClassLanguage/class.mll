{
open Types
 
type token = 
	| VISIBILITY of visibility
	| ABSTRACT of abstraction 
  | CLASS
  | CLASS_NAME of string
  | OPENING_BRACKET
  | CLOSING_BRACKET
  | EOF

let findVisibility v =   
			match v with
    	|PUBLIC -> print_string "PUBLIC"
    	|PROTECTED -> print_string "PROTECTED"
    	|PRIVATE -> print_string "PRIVATE"

let printLexeme = function
    | EOF     -> print_string "EOF"
    | VISIBILITY v -> findVisibility v
    | CLASS  -> print_string "CLASS"
    | CLASS_NAME s -> print_string s
    | OPENING_BRACKET -> print_string "OPENING_BRACKET"
		| CLOSING_BRACKET -> print_string "CLOSING_BRACKET"
}

let className = ['A'-'Z']['0'-'9' 'a'-'z' '_' '$' 'A'-'Z']*

let space = [' ' '\t' '\n']

rule nextToken = parse
  | eof {EOF}
  | "public" {VISIBILITY PUBLIC}
  | "protected" {VISIBILITY PROTECTED}
  | "private" {VISIBILITY PRIVATE}
  | space+  { nextToken lexbuf }
  | "class" {CLASS}
  | className as className    { CLASS_NAME className }
  | '{' {OPENING_BRACKET}
  | '}' {CLOSING_BRACKET}

{
let rec examineAll lexbuf =
    let res = nextToken lexbuf in
    printLexeme res;
    print_string " ";
    match res with
    | EOF -> ()
    | _   -> examineAll lexbuf
} 
