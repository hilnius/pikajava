%{
open Types
open Location
open ExitManagement
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
| enum=enumDeclaration { enum }
| error {print_string "\027[31mError: unable to parse "; print_token_full (symbol_loc $startpos $endpos); setExitCodeValue 2; print_string "\027[0m"; ErrorDecl ("Error : Invalid Declaration\n")}

enumDeclaration:
| cm=modifiers? ENUM id=IDENTIFIER ifs=implements eb=enumBody { EnumTree({ objectType=Enum; modif=cm; inh=ifs; enumName=Identifier id; con=eb }); }
enumBody:
| OPENING_BRACKET ec=enumConstants? COMMA? ebd=enumBodyDeclarations CLOSING_BRACKET { { enumConstants=ec; con=ebd } }
enumConstants:
| e=enumConstant { [e] }
| es=enumConstants COMMA e=enumConstant { es @ [e] }
enumConstant:
| an=annotations? id=IDENTIFIER ar=arguments? cb=classContentDeclarations { { annotations=an; identifier=(Identifier id); arguments=ar; classBody=cb } }
arguments:
| OPENING_PARENTHESIS  CLOSING_PARENTHESIS { [] } (* FIXME *)
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
| STATIC b=blockDeclaration { Initializer({iniType=Static;con=b}) }
| methodDecl=methodDeclaration {methodDecl}
| objectDecl=classDeclaration {ObjectTree(objectDecl)}
| block=blockDeclaration {Initializer({iniType=NonStatic;con=block})}
%%
