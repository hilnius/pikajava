%{
open Types
open Location
%}

%start  classDeclaration
%type <Types.objectTree>  classDeclaration
%%
(*TODO GENERICS???? *)
classDeclaration:
| vis=visibility abs=abstraction fin=finality CLASS className=IDENTIFIER params=parameters inh=inherits impl=implements  OPENING_BRACKET
	{ClassTree({objectType=Class;vis=vis;abs=abs;fin=fin;parameters=params;inh=inh;impl=impl;className=Identifier className;con=None});}
| vis=visibility INTERFACE interfaceName=IDENTIFIER params=parameters inh=inheritsInterface OPENING_BRACKET
	{InterfaceTree({objectType=Interface;vis=vis;inh=inh;parameters=params;interfaceName=Identifier interfaceName;con=None});}
| vis=visibility ENUM enumName=IDENTIFIER  inh=inheritsInterface OPENING_BRACKET	
	{EnumTree({objectType=Enum;vis=vis;inh=inh;enumName=Identifier enumName;con=None});}
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
inheritsInterface:
| EXTENDS completeInterf=interface {Some(completeInterf)}
implements:
| IMPLEMENTS completeInterf=interface {Some(completeInterf)}
| {None}
interface:
| className=IDENTIFIER COMA interf=interface {(Identifier className)::interf}
| className=IDENTIFIER {[Identifier(className)]}

parameters:
| OPENING_CHEVRON paramList=parameterList CLOSING_CHEVRON {Some(paramList)}
| {None}
parameterList:
| parameterName = IDENTIFIER COMA paramList = parameterList {({name= Identifier(parameterName);extends=None})::paramList}
| parameterName = IDENTIFIER EXTENDS className = IDENTIFIER COMA paramList=parameterList { ({name= Identifier(parameterName); extends = Some(className)})::paramList}
| parameterName = IDENTIFIER EXTENDS className = IDENTIFIER {[{name= Identifier(parameterName); extends = Some(className)}]}
| parameterName= IDENTIFIER {[{name=Identifier parameterName;extends=None}]}

%%
