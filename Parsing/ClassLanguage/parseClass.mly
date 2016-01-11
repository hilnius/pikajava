%{
open Types
open Location
open ExitManagement
%}



%start  objectDeclaration
%type <Types.classContentTree>  objectDeclaration
%%
objectDeclaration:
(*| interfaceDecl=interfaceDeclaration { ModifiedDeclaration(None, (InterfaceDeclaration interfaceDecl)) }*)
(*| classDecl=classDeclaration { classDecl }*)
(*| enum=enumDeclaration { enum } *)
| error {print_string "\027[31mError: unable to parse content declaration"; print_token_full (symbol_loc $startpos $endpos); setExitCodeValue 2; print_string "\027[0m"; EmptyContent}

unmodifiedClassDeclaration:
| CLASS i=identifier tp=typeParameters? s=super? ifs=interfaces? cb=classBody { ClassDeclaration({objectType=Class;parameters=tp;super=s;interfaces=ifs;className=i;con=cb}) }
interfaces:
| IMPLEMENTS itl=interfaceTypeList { itl }
interfaceTypeList:
| it=interfaceType { [it] }
| itl=interfaceTypeList COMMA it=interfaceType { itl @ [it] }

super:
| EXTENDS ct=classType { Extends(ct) }

unmodifiedInterfaceDeclaration:
| INTERFACE i=identifier tp=typeParameters? ext=extendsInterfaces? body=interfaceBody { InterfaceDeclaration({objectType=Interface; inh=ext; interfaceName=i; parameters=tp; con=body}) }
| AT_INTERFACE i=identifier a=annotationTypeBody { AnnotationType(i,a) }

annotationTypeBody:
| OPENING_BRACE ated=annotationTypeElementDeclarations? CLOSING_BRACE { ated }
annotationTypeElementDeclarations:
| a=classMemberDeclaration { [a] }
| ass=annotationTypeElementDeclarations a=classMemberDeclaration { ass @ [a]}
unmodifiedAnnotationTypeElementDeclaration:
| t=typed id=identifier OPENING_PARENTHESIS CLOSING_PARENTHESIS dv=defaultValue? SEMICOLON { AnnotationTypeElement(t,id,dv) }
defaultValue:
| DEFAULT e=elementValue { e }

extendsInterfaces:
| EXTENDS intType=interfaceType {[intType]}
| extendInt=extendsInterfaces COMMA intType=interfaceType {extendInt @ [intType]}

interfaceBody:
| OPENING_BRACE interfMembers=interfaceMemberDeclarations? CLOSING_BRACE {interfMembers}

interfaceMemberDeclarations:
| interf=classMemberDeclaration {[interf]}
| interfs=interfaceMemberDeclarations interf=classMemberDeclaration {interfs @ [interf]}

unmodifiedEnumDeclaration:
| ENUM id=identifier ifs=interfaces? eb=enumBody { EnumDeclaration({ objectType=Enum; inh=ifs; enumName=id; con=eb }); }
enumBody:
| OPENING_BRACE ec=enumConstants? COMMA? ebd=enumBodyDeclarations? CLOSING_BRACE { { enumConstants=ec; con=ebd } }
enumConstants:
| e=enumConstant { [e] }
| es=enumConstants COMMA e=enumConstant { es @ [e] }
enumConstant:
| an=annotations? id=identifier ar=arguments? cb=classBody? { { annotations=an; identifier=id; arguments=ar; classBody=cb } }
enumBodyDeclarations:
| SEMICOLON cb=classBodyDeclarations? { cb }

%public classBody:
| OPENING_BRACE cbd=classBodyDeclarations? CLOSING_BRACE { cbd }
classBodyDeclarations:
| cbd=classBodyDeclaration { [cbd] }
| cbds=classBodyDeclarations cbd=classBodyDeclaration { cbds @ [cbd] }
classBodyDeclaration:
| cbd=instanceInitializer { cbd }
| cbd=staticInitializer { cbd }
| cbd=classMemberDeclaration { cbd }
%public classMemberDeclaration:
| am=anyModifiers? md=modifiedDeclaration { print_string "reading modifiers..."; ModifiedDeclaration(am, md) }
%inline modifiedDeclaration:
| cmd=methodDeclaration { cmd }
| cmd=fieldDeclaration { cmd }
| cmd=unmodifiedClassDeclaration { cmd }
| cmd=unmodifiedInterfaceDeclaration { cmd }
| cmd=unmodifiedEnumDeclaration { cmd }
| cmd=unmodifiedPackageDeclaration { cmd }
(*| cmd=unmodifiedAnnotationTypeElementDeclaration { cmd } *)
| SEMICOLON { EmptyDeclaration }
%inline instanceInitializer:
| b=blockDeclaration { InstanceInitializer b }
%inline staticInitializer:
| STATIC b=blockDeclaration { StaticInitializer b }

%public constructorBody:
| OPENING_BRACE bl=blockStatements? CLOSING_BRACE {
    match bl with
    | None -> Block( [] )
    | Some(l) -> Block( l )
  }
| OPENING_BRACE nwta=nonWildTypeArguments? THIS OPENING_PARENTHESIS al=argumentList? CLOSING_PARENTHESIS SEMICOLON bl=blockStatements? CLOSING_BRACE {
    match bl with
    | None -> Block( [ConstructorInitialization (nwta, This, al)] )
    | Some(bl) -> Block( [ConstructorInitialization (nwta, This, al)] @ bl )
  }
| OPENING_BRACE nwta=nonWildTypeArguments? SUPER OPENING_PARENTHESIS al=argumentList? CLOSING_PARENTHESIS SEMICOLON bl=blockStatements? CLOSING_BRACE {
    match bl with
    | None -> Block( [ConstructorInitialization (nwta, This, al)] )
    | Some(bl) -> Block( [ConstructorInitialization (nwta, This, al)] @ bl )
  }
%public %inline nonWildTypeArguments:
| OPENING_CHEVRON rtl=referenceTypeList CLOSING_CHEVRON { NonWildTypeArguments(rtl) }
referenceTypeList:
| rt=referenceType { [rt] }
| rtl=referenceTypeList COMMA rt=referenceType { rtl @ [rt] }

fieldDeclaration:
| aType=integralType varDecls=variableDeclarators SEMICOLON { FieldDeclaration(TypePrimitive(aType), varDecls) }

%public methodDeclaration:
| mh=methodHeader mb=methodBody { MethodDeclaration(mh, mb) }
methodHeader:
| tp=typeParameters? mr=methodHeaderRest {
    match mr with
    | Method { parameters=_; returnType=rt; methodDeclarator=md; thr=th; con=(Some (Block [])) } -> Method { parameters=tp; returnType=rt; methodDeclarator=md; thr=th; con=(Some (Block [])) }
    | Constructor { parameters=_; methodDeclarator=md; thr=th; con=(Some (Block [])) } -> Constructor { parameters=tp; methodDeclarator=md; thr=th; con=(Some (Block [])) }
  }
methodHeaderRest:
| rt=resultType md=methodDeclarator th=throws? { print_string "method\n"; Method { parameters=None; returnType=rt; methodDeclarator=md; thr=th; con=(Some (Block [])) } }
| md=methodDeclarator th=throws? { print_string "constructor\n"; Constructor { parameters=None; methodDeclarator=md; thr=th; con=(Some (Block [])) } }
methodBody:
| b=constructorBody { Some(b) }
| SEMICOLON { None }
%public resultType:
| t=typed { t }
| VOID { TypePrimitive(Void) }
%public methodDeclarator:
| id=identifier OPENING_PARENTHESIS fpl=formalParameterList? CLOSING_PARENTHESIS { { identifier=id; parameters=fpl } }
| error {print_string "\027[31mError: unable to parse method"; print_token_full (symbol_loc $startpos $endpos); setExitCodeValue 2; print_string "\027[0m"; raise (SyntaxError "yoplo")}

%public formalParameterList:
| lfp=lastFormalParameter { [lfp] }
| fp=formalParameters COMMA lfp=lastFormalParameter { fp @ [lfp] }
formalParameters:
| fp=formalParameter { [fp] }
| fps=formalParameters COMMA fp=formalParameter { fps @ [fp] }
%public variableModifiers:
| vm=variableModifier { [vm] }
| vms=variableModifiers vm=variableModifier { vms @ [vm] }
variableModifier:
| FINAL { Finality(Final) }
| an=annotation { Annotation(an) }
lastFormalParameter:
| vm=variableModifiers tv=typedVariadic? vdi=variableDeclaratorId { let (a,b) = vdi in { modifiers=vm; typed=tv; declarator=(a,b,None) } }
| fp=formalParameter { fp }
typedVariadic:
| t=typed VARIADIC { VariadicType(t) }
%public anyModifiers:
| mm=anyModifier { [mm] }
| mms=anyModifiers mm=anyModifier { mms @ [mm] }
%public anyModifier:
| an=annotation { Annotation(an) }
| PUBLIC { Visibility(Public) }
| PROTECTED { Visibility(Protected) }
| PRIVATE { Visibility(Private) }
| ABSTRACT { Abstraction(Abstract) }
| STATIC { Staticity(Static) }
| FINAL { Finality(Final) }
| SYNCHRONIZED { Synchronization(Synchronized) }
| NATIVE { Nativity(Native) }
| STRICTFP { StrictFpity(StrictFp) }
| TRANSIENT {Transient(Transient)}
| VOLATILE {Volatile(Volatile)}
%public throws:
| THROWS etl=exceptionTypeList { etl }
exceptionTypeList:
| et=exceptionType { [et] }
| ets=exceptionTypeList COMMA et=exceptionType { ets @ [et] }
exceptionType:
| ct=classType { ExceptionClassOrInterfaceType(ct) }
| tv=typeVariable { ExceptionTypeVariable(tv) }

%%
