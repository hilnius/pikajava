%{
open Types
open Location
%}

%start  classDeclaration
%type <Types.objectTree>  classDeclaration
%%
classDeclaration:
| modifs=modifiersList CLASS className=IDENTIFIER params=parametersDeclaration inh=inherits impl=implements  OPENING_BRACKET con=classContentDeclarations 
	CLOSING_BRACKET
	{ClassTree({objectType=Class;modif=modifs;parameters=params;inh=inh;impl=impl;className=Identifier className;con=con});}
| modifs=modifiersList INTERFACE interfaceName=IDENTIFIER params=parametersDeclaration inh=inheritsInterface OPENING_BRACKET con=classContentDeclarations CLOSING_BRACKET
	{InterfaceTree({objectType=Interface;modif=modifs;inh=inh;parameters=params;interfaceName=Identifier interfaceName;con=con});}
| modifs=modifiersList ENUM enumName=IDENTIFIER  inh=inheritsInterface OPENING_BRACKET con=classContentDeclarations CLOSING_BRACKET
	{EnumTree({objectType=Enum;modif=modifs;inh=inh;enumName=Identifier enumName;con=con});}
| error {print_string "Error : Invalid Declaration\n";print(symbol_loc $startpos $endpos);Empty}

modifiersList:
| modifsList=modifiers {Some(modifsList)}
| {None}
modifiers:
| modif=modifierClass modifs=modifiers {(modif)::modifs}
| modif=modifierClass {[modif]}
%public modifierClass:
| vis=visibility {Visibility vis}
| abs=abstraction {Abstraction abs}
| fin=finality {Finality fin}
| sta=staticity {Staticity sta}
| strict=strictfp {StrictFpity strict}	
visibility:
|PUBLIC {Public}
|PRIVATE {Private}
|PROTECTED {Protected}
abstraction:
|ABSTRACT {Abstract}
staticity:
|STATIC {Static}
finality:
|FINAL {Final}
strictfp:
| STRICTFP {StrictFp}
inherits:
| EXTENDS parentName=IDENTIFIER parameters=parametersDeclaration {Some(Parent({name=parentName;parameters=parameters}))}
| {None}
inheritsInterface:
| EXTENDS completeInterf=interface {Some(completeInterf)}
| {None}
implements:
| IMPLEMENTS completeInterf=interface {Some(completeInterf)}
| {None}
interface:
| className=IDENTIFIER parameters=parametersDeclaration COMMA interf=interface {(Parent({name=className; parameters=parameters}))::interf}
| className=IDENTIFIER parameters=parametersDeclaration {[Parent({name=className; parameters=parameters})]}

classContentDeclarations:
| classContentList=classContentList {Some(classContentList)}
| {None}
classContentList:
| classContentDecl=classContentDeclaration classContentList=classContentList  {(classContentDecl)::classContentList}
| classContentDecl=classContentDeclaration {[classContentDecl]}
classContentDeclaration:
| STATIC block=blockDeclaration {Initializer({iniType=Static;con=block})}
| block=blockDeclaration {Initializer({iniType=NonStatic;con=block})}
| methodDecl=methodDeclaration {methodDecl} 
| objectDecl=classDeclaration {ObjectTree(objectDecl)}
%%
