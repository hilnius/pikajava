%{
open Types
open Location
%}

%start  classDeclaration
%type <Types.classTree>  classDeclaration
%%
classDeclaration:
| vis=visibility abs=abstraction fin=finality CLASS className=IDENTIFIER  inh=inherits impl=implements  OPENING_BRACKET
	{ClassTree({vis=vis;abs=abs;fin=fin;inh=inh;impl=impl;className=Identifier className;con=None});}
| error {print_string "Error : Invalid Class Declaration\n";print(symbol_loc $startpos $endpos);Empty}	
%public visibility:
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
| EXTENDS parentName=IDENTIFIER {Some parentName}
| {None}
implements:
| IMPLEMENTS completeInterf=interface {Some(completeInterf)}
| {None}
interface:
| className=IDENTIFIER COMA interf=interface {(Identifier className)::interf}
| className=IDENTIFIER {[Identifier(className)]}
%%
