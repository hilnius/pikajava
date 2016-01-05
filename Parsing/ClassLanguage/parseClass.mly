%{
open Types
open Location
%}

%start  classDeclaration
%type <Types.objectTree>  classDeclaration
%%
(*TODO GENERICS???? *)
classDeclaration:
| modifs=modifiers CLASS className=IDENTIFIER params=parametersDeclaration inh=inherits impl=implements  OPENING_BRACKET con=methodsDeclarations 
	CLOSING_BRACKET
	{ClassTree({objectType=Class;modif=Some(modifs);parameters=params;inh=inh;impl=impl;className=Identifier className;con=con});}
| modifs=modifiers INTERFACE interfaceName=IDENTIFIER params=parametersDeclaration inh=inheritsInterface OPENING_BRACKET
	{InterfaceTree({objectType=Interface;modif=Some(modifs);inh=inh;parameters=params;interfaceName=Identifier interfaceName;con=None});}
| modifs=modifiers ENUM enumName=IDENTIFIER  inh=inheritsInterface OPENING_BRACKET	
	{EnumTree({objectType=Enum;modif=Some(modifs);inh=inh;enumName=Identifier enumName;con=None});}
| error {print_string "Error : Invalid Class Declaration\n";print(symbol_loc $startpos $endpos);Empty}

modifiers:
| modif=modifierClass modifs=modifiers {(modif)::modifs}
| modif=modifierClass {[modif]}

modifierClass:
| vis=visibility {Visibility vis}
| abs=abstraction {Abstraction abs}
| fin=finality {Finality fin}
| sta=staticity {Staticity sta}
| strict=strictfp {StrictFpity strict}	
%public visibility:
|PUBLIC {Public}
|PRIVATE {Private}
|PROTECTED {Protected}
%public abstraction:
|ABSTRACT {Abstract}
%public staticity:
|STATIC {Static}
%public finality:
|FINAL {Final}
%public strictfp:
| STRICTFP {StrictFp}
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

methodsDeclarations:
| methodsList=methodDeclarationsList {Some(methodsList)}
| {None}
methodDeclarationsList:
| methDecl=methodDeclaration methDeclList=methodDeclarationsList  {(methDecl)::methDeclList}
| methDecl=methodDeclaration {[methDecl]}
%%
