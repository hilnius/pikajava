%{
open Types
open Location
open ExitManagement
%}

%start someMethodDeclaration
%type <Types.classContentTree> someMethodDeclaration
%%


someMethodDeclaration:
(*|abstractMethodDecl=abstractMethodDeclaration {abstractMethodDecl}*)
| methodDecl=methodDeclaration { methodDecl }
%public methodDeclaration :
| mh=methodHeader mb=methodBody {
  let { parameters=tp; modif=mm; returnType=rt; methodDeclarator=md; thr=th } = mh in
      MethodDeclaration({ parameters=tp; modif=mm; returnType=rt; methodDeclarator=md; thr=th; con=mb })
  }
methodHeader:
| mm=anyModifiers? tp=typeParameters? rt=resultType md=methodDeclarator th=throws? { { parameters=tp; modif=mm; returnType=rt; methodDeclarator=md; thr=th; con=(Some (Block [])) } }
methodBody:
| b=blockDeclaration { Some(b) }
| SEMICOLON { None }
%public resultType:
| t=typed { t }
| VOID { TypePrimitive(Void) }
%public methodDeclarator:
| id=identifier OPENING_PARENTHESIS fpl=formalParameterList? CLOSING_PARENTHESIS { { identifier=id; parameters=fpl } }
formalParameterList:
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
