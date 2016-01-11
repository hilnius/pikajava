%{
open Types
open Location
open ExitManagement
%}



%start  objectDeclaration
%type <Types.objectTree>  objectDeclaration
%%
objectDeclaration:
(*| interfaceDecl=interfaceDeclaration { interfaceDecl}*)
| classDecl=classDeclaration { ClassTree classDecl }
(*| enum=enumDeclaration { enum } *)
(*| error {print_string "\027[31mError: unable to parse "; print_token_full (symbol_loc $startpos $endpos); setExitCodeValue 2; print_string "\027[0m"; ErrorDecl ("Error : Invalid Declaration\n")}*)

classDeclaration:
| cm=classModifiers? CLASS i=identifier tp=typeParameters? s=super? ifs=interfaces? cb=classBody { ClassDeclaration({objectType=Class;modif=cm;parameters=tp;super=s;interfaces=ifs;className=i;con=cb}) }

classModifiers:
| cm=anyModifiers { cm }

interfaces:
| IMPLEMENTS itl=interfaceTypeList { itl }
interfaceTypeList:
| it=interfaceType { [it] }
| itl=interfaceTypeList COMMA it=interfaceType { itl @ [it] }

super:
| EXTENDS ct=classType { Extends(ct) }

(*interfaceDeclaration:
| modifs=modifiersList INTERFACE interfaceName=identifier params=typeParameters inh=inheritsInterface OPENING_BRACE con=interfaceMemberDeclarations? CLOSING_BRACE
	{InterfaceTree({objectType=Interface;modif=modifs;inh=inh;parameters=params;interfaceName=interfaceName;con=con});}
interfaceMemberDeclarations:
| interf=interfaceMemberDeclaration {[interf]}
| interfs=interfaceMemberDeclarations interf=interfaceMemberDeclaration {interfs @ [interf]}

interfaceMemberDeclaration:
(*TODO |constantDeclaration*)
(*|absMethod=abstractMethodDeclaration {absMethod}*)
|classDecl=classDeclaration {ObjectTree classDecl}
|interfDecl=interfaceDeclaration {ObjectTree interfDecl}*)

(*enumDeclaration:
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
| anno=annotation {Annotation anno}*)

classBody:
| OPENING_BRACE cbd=classBodyDeclarations? CLOSING_BRACE { cbd }
classBodyDeclarations:
| cbd=classBodyDeclaration { [cbd] }
| cbds=classBodyDeclarations cbd=classBodyDeclaration { cbds @ [cbd] }
classBodyDeclaration:
| cbd=classMemberDeclaration { cbd }
(*| cbd=instanceInitializer { cbd } *)
(*| cbd=staticInitializer { cbd } *)
(*| cbd=constructorDeclaration { cbd } *)
classMemberDeclaration:
(*| cmd=fieldDeclaration { cmd }*)
| cmd=methodDeclaration { cmd }
| cmd=classDeclaration { cmd }
(*| cmd=interfaceDeclaration { cmd }*)
| SEMICOLON { EmptyContent }

inherits:
| EXTENDS parentName=identifier parameters=typeParameters {Some(Parent({name=parentName;parameters=parameters}))}
| {None}
inheritsInterface:
| EXTENDS completeInterf=interface {Some(completeInterf)}
| {None}
implements:
| IMPLEMENTS completeInterf=interface {Some(completeInterf)}
| {None}
interface:
| className=identifier parameters=typeParameters COMMA interf=interface {(Parent({name=className; parameters=parameters}))::interf}
| className=identifier parameters=typeParameters {[Parent({name=className; parameters=parameters})]}


%%
