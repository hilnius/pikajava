%{
open Types

%}

%token CLASS OPENING_BRACKET CLOSING_BRACKET EOF
%token PUBLIC PRIVATE PROTECTED 
%token ABSTRACT 
%token FINAL 
%token<string> CLASS_NAME
%token EXTENDS IMPLEMENTS
%token COMA


%start classDeclaration
%type <Types.classTree> classDeclaration
%%
classDeclaration:
| vis=visibility abs=abstraction fin=finality CLASS className=CLASS_NAME  inh=inherits impl=implements  OPENING_BRACKET 
	{ClassTree({vis=vis;abs=abs;fin=fin;inh=inh;impl=impl;className=Identifier className;con=None});}
visibility:
|PUBLIC {Public}
|PRIVATE {Private}
|PROTECTED {Protected}
| {Private_Package}
abstraction:
|ABSTRACT {Abstract}
| {Concrete}
finality:
|FINAL {Final}
| {Extendable}
inherits:
| EXTENDS parentName=CLASS_NAME {Some parentName}
| {None} 
implements:
| IMPLEMENTS completeInterf=interface {Some(completeInterf)}
| {None}
interface:
| className=CLASS_NAME COMA interf=interface {(Identifier className)::interf}
| className=CLASS_NAME {[Identifier(className)]}
%%	


