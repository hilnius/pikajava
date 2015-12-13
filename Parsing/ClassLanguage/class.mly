%{
open Types

type classTree = ClassTree of (visibility * abstraction * identifier * content);;

%}

%token CLASS OPENING_BRACKET CLOSING_BRACKET EOF
%token<visibility> VISIBILITY
%token<abstraction> ABSTRACTION
%token<finality> FINALITY
%token<string> CLASS_NAME


%start classDeclaration
%type <classTree> classDeclaration

%%
classDeclaration:
| decl = visibility EOF {decl}
visibility:
| vis = VISIBILITY abstraction {}
abstraction : 
| abs = ABSTRACTION finality  {}
finality:
| fin = FINALITY className  {}
className:
| cln = CLASS_NAME {}  
%%	


