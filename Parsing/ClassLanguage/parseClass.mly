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

%public classDeclaration:
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
	{ (Method { parameters=tp ; modif=abstrModif; returnType=rt; methodDeclarator=methDecl; thr=exc; con=None}, None) }
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

(* Avoid conflict*)
(*constantModifiers:
|constModif=constantModifier {[constModif]}
|constModif=constantModifier constModifs=constantModifiers {constModifs @ [constModif] }

constantModifier:
|any=anyModifier {any}*)

%public classBody:
| OPENING_BRACE cbd=classBodyDeclarations? CLOSING_BRACE { cbd }
classBodyDeclarations:
| cbd=classBodyDeclaration { [cbd] }
| cbds=classBodyDeclarations cbd=classBodyDeclaration { cbds @ [cbd] }
classBodyDeclaration:
| cbd=instanceInitializer { print_string "coucou"; cbd }
| cbd=staticInitializer {  print_string "coucou2";cbd }
| cbd=classMemberDeclaration {  print_string "coucou4";cbd }
classMemberDeclaration:
| cmd=fieldDeclaration { FieldDeclaration cmd }
| cmd=methodDeclaration { cmd }
| cmd=classDeclaration {  cmd }
| cmd=interfaceDeclaration { InterfaceDeclaration cmd }
| SEMICOLON { EmptyContent }
instanceInitializer:
| b=blockDeclaration { InstanceInitializer b }
staticInitializer:
| STATIC b=blockDeclaration { StaticInitializer b }
%public constructorBody:
| OPENING_BRACE bl=blockStatements? CLOSING_BRACE {
    match bl with
    | None -> Block( [] )
    | Some(l) -> Block( l )
  }
| OPENING_BRACE eci=explicitConstructorInvocation? bl=blockStatements? CLOSING_BRACE {
    let eciBlock = match eci with
    | None -> []
    | Some(ecib) -> [ConstructorInitialization ecib]
    in
    match bl with
    | None -> Block( eciBlock )
    | Some(l) -> Block( eciBlock @ l )
  }
%inline explicitConstructorInvocation:
| nwta=nonWildTypeArguments? THIS OPENING_PARENTHESIS al=argumentList? CLOSING_PARENTHESIS SEMICOLON {
    (nwta, This, al)
  }
| nwta=nonWildTypeArguments? SUPER OPENING_PARENTHESIS al=argumentList? CLOSING_PARENTHESIS SEMICOLON {
    (nwta, Super, al)
  }
(*| p=primary nwta=nonWildTypeArguments? SUPER OPENING_PARENTHESIS al=argumentList? CLOSING_PARENTHESIS SEMICOLON { } *)
%public %inline nonWildTypeArguments:
| OPENING_CHEVRON rtl=referenceTypeList CLOSING_CHEVRON { NonWildTypeArguments(rtl) }
referenceTypeList:
| rt=referenceType { [rt] }
| rtl=referenceTypeList COMMA rt=referenceType { rtl @ [rt] }

fieldDeclaration:
| fieldModifs=anyModifiers? aType=typed varDecls=variableDeclarators SEMICOLON {{modif= fieldModifs ; varDecl=varDecls}}


%%
