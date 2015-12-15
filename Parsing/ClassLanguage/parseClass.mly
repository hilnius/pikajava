%{
open Types

%}

%token CLASS OPENING_BRACKET CLOSING_BRACKET EOF
%token PUBLIC PRIVATE PROTECTED 
%token ABSTRACT CONCRETE
%token FINAL EXTENDABLE
%token<string> CLASS_NAME

%start classDeclaration
%type <Types.classTree> classDeclaration
%%
classDeclaration:
| vis=visibility abs=abstraction fin=finality CLASS className=CLASS_NAME OPENING_BRACKET {ClassTree(vis,abs,fin,IDENTIFIER className ,Empty);}
visibility:
|PUBLIC {PUBLIC}
|PRIVATE {PRIVATE}
|PROTECTED {PROTECTED}
| {PRIVATE_PACKAGE}
abstraction:
|ABSTRACT {ABSTRACT}
| {CONCRETE}
finality:
|FINAL {FINAL}
| {EXTENDABLE}
%%	


