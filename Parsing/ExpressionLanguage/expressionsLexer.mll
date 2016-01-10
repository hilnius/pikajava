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
  | '!' { EXCLAMATION_MARK }
  | '~' { TILD }

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
  | '<' { LESSTHAN }
  | '>' { GREATERTHAN }
  | '.' { DOT }
  | ',' { COMMA }
  | "<=" { LESSEQUALTHAN }
  | ">=" { GREATEREQUALTHAN }

  | '(' { PAROPEN }
  | ')' { PARCLOSE }
  | '[' { BRACKETOPEN }
  | ']' { BRACKETCLOSE }
  | '{' { BRACEOPEN }
  | '}' { BRACECLOSE }


  | '(' { PAROPEN }
  | ')' { PARCLOSE }
  | '[' { BRACKETOPEN }
  | ']' { BRACKETCLOSE }
  | '{' { BRACEOPEN }
  | '}' { BRACECLOSE }

  | '=' { EQUAL }
  | "+=" { MORE_EQUAL }
  | "-=" { LESS_EQUAL }
  | "*=" { MULTIPLY_EQUAL }
  | "/=" { DIVIDE_EQUAL }
  | "&=" { AND_EQUAL }
  | "|=" { OR_EQUAL }
  | "^=" { XOR_EQUAL }
  | "%=" { MODULO_EQUAL }
  | "<<=" { LEFT_EQUAL }
  | ">>=" { RIGHT_EQUAL }
  | ">>>=" { GGG_EQUAL }

  | "new" { NEW }
  | "void" { VOID }
  | "extends" { EXTENDS }
  | "super" { SUPER }
  | "this" { THIS }
  | "instanceof" { INSTANCEOF }
  | "class" { CLASS }

  | "byte" { BYTE }
  | "short" { SHORTINT }
  | "char" { CHAR }
  | "int" { INTEGER }
  | "long" { LONGINT }
  | "float" { FLOAT }
  | "double" { DOUBLE }
  | "boolean" { BOOLEAN }

  | ['0'-'9']+ as i { INTEGER_NUMERAL (int_of_string i) }
  | ['0'-'9']+ ['.'] ['0'-'9']* as i { FLOATING_POINT_NUMERAL (float_of_string i) }
  | "true" { BOOLEAN_LITERAL true }
  | "false" { BOOLEAN_LITERAL false }
  | "null" { NULL }

  | ['a'-'z' 'A'-'Z'] ['_' '0'-'9' 'a'-'z' 'A'-'Z']* as v { IDENTIFIER v }
