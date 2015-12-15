{
open ParseFile		

let printLexeme = function
    | EOF     -> print_string "EOF"
    | PACKAGE  -> print_string "PACKAGE"
    | IMPORT  -> print_string "IMPORT"
    | STATIC  -> print_string "STATIC"
    | IMPORT_OR_PACKAGE_NAME s -> print_string s
    | SEMICOLON -> print_string ";"
	| DOT -> print_string "."
	| STAR -> print_string "*"
}

let importOrPackageName = ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' '_' '$' 'A'-'Z']*
let space = [' ' '\t' '\n']

rule nextToken = parse
  | eof {EOF}
  | "package" {PACKAGE}
  | "import" {IMPORT}
  | "static" {STATIC}
  | importOrPackageName as name {IMPORT_OR_PACKAGE_NAME name}
  | ";" {SEMICOLON}
  | "." {DOT}
  | "*" {STAR}
  | space+  { nextToken lexbuf }
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
