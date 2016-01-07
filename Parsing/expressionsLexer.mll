{
}
let space = [' ' '\t' '\n']
rule nexttoken = parse
    space+ { nexttoken lexbuf }
  | eof { EOF }
  | '?' { QUESTION_MARK }
  | ':' { TWO_DOTS }
  | "++" { MOREMORE }
  | "--" { LESSLESS }
  | '+' { MORE }
  | '-' { LESS }
  | "||" { OR }
  | "&&" { AND }
  | '|' { BITOR }
  | '^' { BITXOR }
  | '&' { BITAND }
  | "==" { EQUALEQUAL }
  | "!=" { DIFFERENT }
  | "<<" { SHIFTLEFT }
  | ">>" { SHIFTRIGHT }
  | ">>>" { GGG }
  | '*' { MULTIPLY }
  | '/' { DIVIDE }
  | '%' { MODULO }
  | '<' { LESSERTHAN }
  | '>' { GREATERTHAN }
  | '.' { DOT }
  | ',' { COMMA }
  | '<' { LESSERTHAN }
  | '(' { PAROPEN }
  | ')' { PARCLOSE }
  | '=' { EQUAL }
  | "+=" { MOREEQUAL }
  | "-=" { LESSEQUAL }
  | "extends" { EXTENDS }
  | "super" { SUPER }
  | "instanceof" { INSTANCEOF }
  | "int" { INTEGER }
  | "float" { FLOAT }
  | "double" { DOUBLE }
  | ['0'-'9']+ as i { INTEGER_NUMERAL (int_of_string i) }
  | ['0'-'9']+ ['.'] ['0'-'9']* as i { FLOATING_POINT_NUMERAL (float_of_string i) }
  | ['a'-'z' 'A'-'Z'] ['_' '0'-'9' 'a'-'z' 'A'-'Z']* as v { IDENTIFIER v }
