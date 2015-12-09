%{

  type operator =
      ADD
    | SUB
    | MUL
    | DIV;;
  type numberOrVariable =
      Number of int
    | Variable of string;;
  type operation =
      Computation of (operation * operator * operation)
    | Value of numberOrVariable;;

%}

%token EOF ADD SUB MUL DIV

%token <int> INT
%token <string> VAR

%start formule
%type <operation> formule
%%
formule:
| c=primaryOperation EOF { c }
primaryOperation:
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
