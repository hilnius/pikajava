%{
  type assignmentOperator =
      Equal
    | MoreEqual
    | MiunsEqual
  type variableDeclarators =
      VariableId of string
    | VariableDeclId of (VariableId * variableInit)
  type variableInit =
      Expression of expression
  type expression = 
      Expression1 of (literal)
  type primary =
      Literal of literal
  type literal =
      IntegerLiteral of integerLiteral
    | FloatingPointLiteral of floatingPointLiteral
  type integerLiteral =
      DecimalNumeral of int
  type floatingPointLiteral =
      DecimalFloatingPointNumeral of float

%}

%token EOF
%token EQUAL
%token FINAL 
%token INTEGER FLOAT

%token <int> INTEGER_NUMERAL
%token <float> FLOATING_POINT_NUMERAL
%token <string> IDENTIFIER

%start perform
%type <operation> perform
%%
perform:
| c=expression EOF { c }
expression:
| p=expression1 { p }
expression1:
| p=primary { p }
primary:
| p=literal { p }
literal:
| p=integerLiteral { p }
| p=floatingPointLiteral { p }
integerLiteral:
| p=INTEGER_NUMERAL { DecimalNumeral(p) }
floatingPointLiteral:
| p=FLOATING_POINT_NUMERAL { DecimalFloatingPointNumeral(p) }

| p=primaryOperation ADD s=secondaryOperation { Computation(p, ADD, s) }
| p=primaryOperation SUB s=secondaryOperation { Computation(p, SUB, s) }
| s=secondaryOperation { s }
secondaryOperation:
| s=secondaryOperation MUL vi=var_or_int { Computation(s, MUL, Value(vi)) }
| s=secondaryOperation DIV vi=var_or_int { Computation(s, DIV, Value(vi)) }
| vi=var_or_int { Value(vi) }
var_or_int:
| v=VAR { Variable(v) }
| i=INT { Number(i) }
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
