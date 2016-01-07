%{
open Types
open Location
%}

%start  classDeclaration
%type <Types.objectTree>  classDeclaration
%%
classDeclaration:
| modifs=modifiers CLASS className=IDENTIFIER params=parametersDeclaration inh=inherits impl=implements  OPENING_BRACKET con=classContentDeclarations 
	CLOSING_BRACKET
	{ClassTree(print_string "CLASS MATCHED";{objectType=Class;modif=Some(modifs);parameters=params;inh=inh;impl=impl;className=Identifier className;con=con});}
| modifs=modifiers INTERFACE interfaceName=IDENTIFIER params=parametersDeclaration inh=inheritsInterface OPENING_BRACKET con=classContentDeclarations CLOSING_BRACKET
	{InterfaceTree({objectType=Interface;modif=Some(modifs);inh=inh;parameters=params;interfaceName=Identifier interfaceName;con=con});}
| modifs=modifiers ENUM enumName=IDENTIFIER  inh=inheritsInterface OPENING_BRACKET con=classContentDeclarations CLOSING_BRACKET
	{EnumTree({objectType=Enum;modif=Some(modifs);inh=inh;enumName=Identifier enumName;con=con});}
| error {print_string "Error : Invalid Declaration\n";print(symbol_loc $startpos $endpos);Empty}

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
| EXTENDS parentName=IDENTIFIER {Some parentName}
| {None}
inheritsInterface:
| EXTENDS completeInterf=interface {Some(completeInterf)}
| {None}
implements:
| IMPLEMENTS completeInterf=interface {Some(completeInterf)}
| {None}
interface:
| className=IDENTIFIER COMMA interf=interface {(Identifier className)::interf}
| className=IDENTIFIER {[Identifier(className)]}

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
