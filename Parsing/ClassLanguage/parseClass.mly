%{
open Types
open Location
open ExitManagement
%}



%start  objectDeclaration
%type <Types.classContentTree>  objectDeclaration
%%
objectDeclaration:
| interfaceDecl=interfaceDeclaration { InterfaceDeclaration interfaceDecl}
| classDecl=classDeclaration { classDecl }
| enum=enumDeclaration { enum } 
| error {print_string "\027[31mError: unable to parse content declaration"; print_token_full (symbol_loc $startpos $endpos); setExitCodeValue 2; print_string "\027[0m"; EmptyContent}

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

interfaceDeclaration:
| normalInterface=normalInterfaceDeclaration {normalInterface}
(*| annotInterface=annotationTypeDeclaration {annotInterface}*)
normalInterfaceDeclaration:
modifs=anyModifiers? INTERFACE i=identifier tp=typeParameters? ext=extendsInterfaces? body=interfaceBody 
	{{objectType=Interface; modif=modifs; inh=ext; interfaceName=i; parameters=tp; con=body}}
(* Avoid Conflict*)
(*interfaceModifiers:
| modif=interfaceModifier {[modif]}
| modifs=anyModifiers modif=interfaceModifier {modifs @ [modif]}
interfaceModifier:
| any=anyModifier {any}*)

extendsInterfaces:
| EXTENDS intType=interfaceType {[intType]}
| extendInt=extendsInterfaces COMMA intType=interfaceType {extendInt @ [intType]}

interfaceBody:
| OPENING_BRACE interfMembers=interfaceMemberDeclarations? CLOSING_BRACE {interfMembers}

interfaceMemberDeclarations:
| interf=interfaceMemberDeclaration {[interf]}
| interfs=interfaceMemberDeclarations interf=interfaceMemberDeclaration {interfs @ [interf]}

interfaceMemberDeclaration:
|constDecl=fieldDeclaration {FieldDeclaration constDecl}
|absMethod=abstractMethodDeclaration {MethodDeclaration absMethod}
|classDecl=classDeclaration {classDecl}
|interfDecl=interfaceDeclaration {InterfaceDeclaration interfDecl}
| SEMICOLON {EmptyContent}

abstractMethodDeclaration:
| abstrModif=anyModifiers? tp=typeParameters? rt=resultType methDecl=methodDeclarator exc=throws? SEMICOLON
	{{ parameters=tp ; modif=abstrModif; returnType=rt; methodDeclarator=methDecl; thr=exc; con=None}}
(*abstractMethodModifiers:
| any=anyModifiers {any} *)

(*constantDeclaration:
| constantModifs=anyModifiers? aType=typed  variableDecls=variableDeclarators SEMICOLON
	{{modif= constantModifs ; varDecl=variableDecls}}*)
	
enumDeclaration:
| cm=anyModifiers? ENUM id=identifier ifs=interfaces? eb=enumBody { EnumDeclaration({ objectType=Enum; modif=cm; inh=ifs; enumName=id; con=eb }); }
enumBody:
| OPENING_BRACE ec=enumConstants? COMMA? ebd=enumBodyDeclarations? CLOSING_BRACE { { enumConstants=ec; con=ebd } }
enumConstants:
| e=enumConstant { [e] }
| es=enumConstants COMMA e=enumConstant { es @ [e] }
enumConstant:
| an=annotations? id=identifier ar=arguments? cb=classBody? { { annotations=an; identifier=id; arguments=ar; classBody=cb } }
enumBodyDeclarations:
| SEMICOLON cb=classBodyDeclarations? { cb }
annotations:
| a=annotation { [a] }
| annotations=annotations a=annotation { annotations @ [a] }	
	
(* Avoid conflict*)	
(*constantModifiers:
|constModif=constantModifier {[constModif]}
|constModif=constantModifier constModifs=constantModifiers {constModifs @ [constModif] }

constantModifier:
|any=anyModifier {any}*)


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
| cmd=fieldDeclaration { FieldDeclaration cmd }
| cmd=methodDeclaration { cmd }
| cmd=classDeclaration {  cmd }
| cmd=interfaceDeclaration { InterfaceDeclaration cmd }
| SEMICOLON { EmptyContent }

fieldDeclaration:
| fieldModifs=anyModifiers? aType=typed varDecls=variableDeclarators SEMICOLON {{modif= fieldModifs ; varDecl=varDecls}}
(*
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
| className=identifier parameters=typeParameters {[Parent({name=className; parameters=parameters})]}*)


%%
