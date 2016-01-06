%{
  type assignmentOperator =
      NoneAssignmentOperator
    | Equal
    | EqualMore
    | EqualMinus
  and prefixOp =
      NonePrefix
    | PrefixMoreMore
    | PrefixLessLess
    | PrefixMore
    | PrefixLess
  and postfixOp =
      NonePostfix
    | PostfixMoreMore
    | PostfixLessLess
  and typed =
      NoneType
    | TypeIdentifier of ((identifierArgs list) * int)
    | TypeBasic of (basicType)
  and typeArgumentKind =
      NoneTypeArgument
    | TypeArgumentExtends
    | TypeArgumentSuper
  and typeArgument =
      TypeArgumentType of (typed)
    | TypeArgumentGeneric of (typed * typeArgumentKind)
  and basicType =
      Byte
    | Short
    | Char
    | Integer
    | Long
    | Float
    | Double
    | Boolean
  and identifier =
      NoneIdentifier
    | Identifier of string
  and identifierArgs =
    | IdentifierArgs of (identifier * (typeArgument list))
  and variableDeclarators =
      VariableId of string
  and integerLiteral =
      DecimalNumeral of int
  and floatingPointLiteral =
      DecimalFloatingPointNumeral of float
  and literal =
      IntegerLiteral of integerLiteral
    | FloatingPointLiteral of floatingPointLiteral
  and primary =
      PrimaryLiteral of literal
  and expression3exty =
      Expression3Type of typed
    | Expression3Expr of expression
  and expression3 =
      Expression3 of (primary * (expression3exty list) * (prefixOp list) * (postfixOp list))
  and expression2 =
      Expression2 of (expression3)
  and expression1 =
      NoneExpression1
    | Expression1 of (expression2 * expression * expression1)
  and expression = 
      NoneExpression
    | Expression of (expression1 * assignmentOperator * expression1)
%}

%token EOF

%token EQUAL EQUAL_MORE EQUAL_MINUS
%token QUESTION_MARK TWO_DOTS DOT COMMA GREATERTHAN LESSERTHAN
%token MOREMORE LESSLESS MORE LESS

%token EXTENDS SUPER

%token INTEGER FLOAT DOUBLE

%token <int> INTEGER_NUMERAL
%token <float> FLOATING_POINT_NUMERAL
%token <string> IDENTIFIER

%start perform
%type <expression> perform
%%
perform:
| c=expression EOF { c }
expression:
| p=expression1 { Expression(p, NoneAssignmentOperator, NoneExpression1) }
| p=expression1 o=assignmentOperator s=expression1 { Expression(p, o, s) }
assignmentOperator:
| EQUAL { Equal }
| EQUAL_MORE { EqualMore }
| EQUAL_MINUS { EqualMinus }
prefixop:
| MOREMORE { PrefixMoreMore }
| LESSLESS { PrefixLessLess }
| MORE { PrefixMore }
| LESS { PrefixLess }
postfixop:
| MOREMORE { PostfixMoreMore }
| LESSLESS { PostfixLessLess }
expression1:
| p=expression2 { Expression1(p, NoneExpression, NoneExpression1) }
| p=expression2 QUESTION_MARK e=expression TWO_DOTS s=expression1 { Expression1(p, e, s) }
expression2:
| p=expression3 { Expression2(p) }
expression3:
| f=prefixop p=expression3 { let Expression3(lit, e, l, post) = p in Expression3(lit, e, f::l, post) }
| f=expression3exty p=expression3 { let Expression3(lit, e, l, post) = p in Expression3(lit, f::e, l, post) }
| p=exprPrimary l=list(postfixop) { Expression3(p, [], [], l) }
expression3exty:
| p=typed { Expression3Type p }
| p=expression { Expression3Expr p }
exprPrimary:
| p=literal { PrimaryLiteral p }
literal:
| p=integerLiteral { IntegerLiteral(p) }
| p=floatingPointLiteral { FloatingPointLiteral(p) }
integerLiteral:
| p=INTEGER_NUMERAL { DecimalNumeral(p) }
floatingPointLiteral:
| p=FLOATING_POINT_NUMERAL { DecimalFloatingPointNumeral(p) }
typed:
| p=separated_list(DOT, typedIdarg) { TypeIdentifier(p, 0) }
| p=basicType { TypeBasic(p) }
typedIdarg:
| p=IDENTIFIER { IdentifierArgs(Identifier(p), []) }
| p=IDENTIFIER LESSERTHAN a=typeArgument GREATERTHAN { IdentifierArgs(Identifier(p), [a]) }
typeArgument:
| p=typed { TypeArgumentType p }
| QUESTION_MARK { TypeArgumentGeneric(NoneType, NoneTypeArgument) }
| QUESTION_MARK EXTENDS p=typed { TypeArgumentGeneric(p, TypeArgumentExtends) }
| QUESTION_MARK SUPER p=typed { TypeArgumentGeneric(p, TypeArgumentSuper) }
basicType:
| p=INTEGER { Integer }
| p=FLOAT { Float }
| p=DOUBLE { Double }
%%
(*%%
formule:
| c=primaryOperation EOF { c }
primaryOperation:
| p=primaryOperation ADD s=secondaryOperation { "(" ^ p ^ " + " ^ s ^ ")" }
| p=primaryOperation SUB s=secondaryOperation { "(" ^ p ^ " - " ^ s ^ ")" }
| s=secondaryOperation { s }
secondaryOperation:
| s=secondaryOperation MUL vi=var_or_int { "(" ^ s ^ " * " ^ vi ^ ")" }
| s=secondaryOperation DIV vi=var_or_int { "(" ^ s ^ " / " ^ vi ^ ")" }
| vi=var_or_int { vi }
var_or_int:
| v=VAR { v }
| i=INT { string_of_int i }
%%*)
