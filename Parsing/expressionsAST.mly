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
  and infixOp =
      NoneInfix
    | InfixOr
    | InfixAnd
    | InfixBitOr
    | InfixBitXor
    | InfixBitAnd
    | InfixEqual
    | InfixDifferent
    | InfixLesserThan
    | InfixGreaterThan
    | InfixLesserEqual
    | InfixGreaterEqual
    | InfixShiftLeft
    | InfixShiftRight
    | InfixGGG
    | InfixMore
    | InfixLess
    | InfixMultiply
    | InfixDivide
    | InfixModulo
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
    | PrimaryIdentifier of (identifier list)
    | PrimaryExpression of expression
  and expression3exty =
      Expression3Type of typed
    | Expression3Expr of expression
  and expression3infix =
      Expression3Infix of (infixOp * expression3)
  and expression3 =
      Expression3 of (primary * (expression3exty list) * (prefixOp list) * (postfixOp list))
  and expression2 =
      Expression2 of (expression3)
    | Expression2Infix of (expression3 * (expression3infix list))
    | Expression2InstanceOf of (expression3 * typed)
  and expression1 =
      NoneExpression1
    | Expression1 of (expression2 * expression * expression1)
  and expression = 
      NoneExpression
    | Expression of (expression1 * assignmentOperator * expression1)
%}

%token EOF

%token EQUAL MOREEQUAL LESSEQUAL
%token OR AND BITOR BITXOR BITAND EQUALEQUAL DIFFERENT LESSEREQUAL GREATEREQUAL SHIFTLEFT SHIFTRIGHT GGG MULTIPLY DIVIDE MODULO
%token QUESTION_MARK TWO_DOTS DOT COMMA GREATERTHAN LESSERTHAN
%token MOREMORE LESSLESS MORE LESS

%token PAROPEN PARCLOSE

%token EXTENDS SUPER INSTANCEOF

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
| MOREEQUAL { EqualMore }
| LESSEQUAL { EqualMinus }
prefixop:
| MOREMORE { PrefixMoreMore }
| LESSLESS { PrefixLessLess }
| MORE { PrefixMore }
| LESS { PrefixLess }
postfixop:
| MOREMORE { PostfixMoreMore }
| LESSLESS { PostfixLessLess }
infixop:
| OR { InfixOr }
| AND { InfixAnd }
| BITOR { InfixBitOr }
| BITXOR { InfixBitXor }
| BITAND { InfixBitAnd }
| EQUALEQUAL { InfixEqual }
| DIFFERENT { InfixDifferent }
| LESSERTHAN { InfixLesserThan }
| GREATERTHAN { InfixGreaterThan }
| LESSEREQUAL { InfixLesserEqual }
| GREATEREQUAL { InfixGreaterEqual }
| SHIFTLEFT { InfixShiftLeft }
| SHIFTRIGHT { InfixShiftRight }
| GGG { InfixGGG }
| MORE { InfixMore }
| LESS { InfixLess }
| MULTIPLY { InfixMultiply }
| DIVIDE { InfixDivide }
| MODULO { InfixModulo }
parExpression:
| PAROPEN p=expression PARCLOSE { p }
expression1:
| p=expression2 { Expression1(p, NoneExpression, NoneExpression1) }
| p=expression2 QUESTION_MARK e=expression TWO_DOTS s=expression1 { Expression1(p, e, s) }
expression2:
| p=expression3 { Expression2(p) }
| p=expression3 INSTANCEOF t=typed { Expression2InstanceOf(p, t) }
| p=expression3 i=expression2infix { Expression2Infix(p, i) }
expression2infix:
| i=infixop p=expression3 { [Expression3Infix(i, p)] }
| i=infixop p=expression3 e=expression2infix { Expression3Infix(i, p)::e }
expression3:
| f=prefixop p=expression3 { let Expression3(lit, e, l, post) = p in Expression3(lit, e, f::l, post) }
| f=expression3exty p=expression3 { let Expression3(lit, e, l, post) = p in Expression3(lit, f::e, l, post) }
| p=exprPrimary l=list(postfixop) { Expression3(p, [], [], l) }
expression3exty:
| p=typed { Expression3Type p }
| p=expression { Expression3Expr p }
exprPrimary:
| p=parExpression { PrimaryExpression p }
| p=literal { PrimaryLiteral p }
| p=exprPrimaryIdentifiers { PrimaryIdentifier(p) }
exprPrimaryIdentifiers:
| p=identifier { [p] }
| p=identifier DOT e=exprPrimaryIdentifiers { p::e }
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
| p=identifier { IdentifierArgs(p, []) }
| p=identifier LESSERTHAN a=separated_nonempty_list(COMMA, typeArgument) GREATERTHAN { IdentifierArgs(p, a) }
typeArgument:
| p=typed { TypeArgumentType p }
| QUESTION_MARK { TypeArgumentGeneric(NoneType, NoneTypeArgument) }
| QUESTION_MARK EXTENDS p=typed { TypeArgumentGeneric(p, TypeArgumentExtends) }
| QUESTION_MARK SUPER p=typed { TypeArgumentGeneric(p, TypeArgumentSuper) }
basicType:
| INTEGER { Integer }
| FLOAT { Float }
| DOUBLE { Double }
identifier:
| p=IDENTIFIER { Identifier(p) }
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
