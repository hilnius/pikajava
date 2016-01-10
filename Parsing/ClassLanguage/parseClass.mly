%{
open Types
open Location
open ExitManagement
%}



%start  objectDeclaration
%type <Types.objectTree>  objectDeclaration
%%
objectDeclaration:
| interfaceDecl=interfaceDeclaration { interfaceDecl}
| classDecl=classDeclaration { classDecl}
| enum=enumDeclaration { enum }
| error {print_string "\027[31mError: unable to parse "; print_token_full (symbol_loc $startpos $endpos); setExitCodeValue 2; print_string "\027[0m"; ErrorDecl ("Error : Invalid Declaration\n")}

classDeclaration:
| modifs=modifiersList CLASS className=identifier params=parametersDeclaration inh=inherits impl=implements  OPENING_BRACE con=classContentDeclarations
	CLOSING_BRACE
	{ClassTree({objectType=Class;modif=modifs;parameters=params;inh=inh;impl=impl;className=className;con=con});}
interfaceDeclaration:
| modifs=modifiersList INTERFACE interfaceName=identifier params=parametersDeclaration inh=inheritsInterface OPENING_BRACE con=interfaceMemberDeclarations? CLOSING_BRACE
	{InterfaceTree({objectType=Interface;modif=modifs;inh=inh;parameters=params;interfaceName=interfaceName;con=con});}
interfaceMemberDeclarations:
| interf=interfaceMemberDeclaration {[interf]}
| interfs=interfaceMemberDeclarations interf=interfaceMemberDeclaration {interfs @ [interf]}

interfaceMemberDeclaration:
(*TODO |constantDeclaration*)
|absMethod=abstractMethodDeclaration {absMethod}
|classDecl=classDeclaration {ObjectTree classDecl}
|interfDecl=interfaceDeclaration {ObjectTree interfDecl}

enumDeclaration:
| cm=modifiers? ENUM id=identifier ifs=implements eb=enumBody { EnumTree({ objectType=Enum; modif=cm; inh=ifs; enumName=id; con=eb }); }
enumBody:
| OPENING_BRACE ec=enumConstants? COMMA? ebd=enumBodyDeclarations CLOSING_BRACE { { enumConstants=ec; con=ebd } }
enumConstants:
| e=enumConstant { [e] }
| es=enumConstants COMMA e=enumConstant { es @ [e] }
enumConstant:
| an=annotations? id=identifier ar=enumConstantsArguments? cb=classContentDeclarations { { annotations=an; identifier=id; arguments=ar; classBody=cb } }
enumConstantsArguments:
| OPENING_PARENTHESIS ar=arguments CLOSING_PARENTHESIS { ar }
enumBodyDeclarations:
| SEMICOLON cb=classContentDeclarations { cb }
|  { None }
annotations:
| a=annotation { [a] }
| annotations=annotations a=annotation { annotations @ [a] }

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
| anno=annotation {Annotation anno}
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
| EXTENDS parentName=identifier parameters=parametersDeclaration {Some(Parent({name=parentName;parameters=parameters}))}
| {None}
inheritsInterface:
| EXTENDS completeInterf=interface {Some(completeInterf)}
| {None}
implements:
| IMPLEMENTS completeInterf=interface {Some(completeInterf)}
| {None}
interface:
| className=identifier parameters=parametersDeclaration COMMA interf=interface {(Parent({name=className; parameters=parameters}))::interf}
| className=identifier parameters=parametersDeclaration {[Parent({name=className; parameters=parameters})]}

classContentDeclarations:
| classContentList=classContentList {Some(classContentList)}
| {None}
classContentList:
| classContentDecl=classContentDeclaration classContentList=classContentList  {(classContentDecl)::classContentList}
| classContentDecl=classContentDeclaration {[classContentDecl]}
classContentDeclaration:
| STATIC b=blockDeclaration { Initializer({iniType=Static;con=b}) }
| methodDecl=methodDeclaration {methodDecl}
| abstractMethodDecl=abstractMethodDeclaration {abstractMethodDecl}
| objectDecl=objectDeclaration {ObjectTree(objectDecl)}
| block=blockDeclaration {Initializer({iniType=NonStatic;con=block})}
%%
