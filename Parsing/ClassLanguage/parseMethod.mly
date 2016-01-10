%{
open Types
open Location
open ExitManagement
%}

%start  someMethodDeclaration
%type <Types.classContentTree>  someMethodDeclaration
%%


someMethodDeclaration:
|abstractMethodDecl=abstractMethodDeclaration {abstractMethodDecl}
|methodDecl=methodDeclaration {methodDecl}
%public methodDeclaration :
| modifs=modifiersMethodList params=parametersDeclaration return=returnType methodName=identifier arguments=arguments exceptions=exceptionDecl block=blockOrAbstract
	{MethodTree({parameters=params; modif=modifs; returnType= return; name= methodName; args=arguments; thr=exceptions; con=block});}
%public abstractMethodDeclaration:
| modifs=modifiersMethodList params=parametersDeclaration return=returnType methodName=identifier arguments=arguments exceptions=exceptionDecl SEMICOLON
	{MethodTree({parameters=params; modif=modifs; returnType= return; name= methodName; args=arguments; thr=exceptions; con=None});}
modifiersMethodList:
| modifsMethod = modifiersMethod {Some(modifsMethod)}
| {None}
modifiersMethod:
| modif=modifierMethod modifs=modifiersMethod {(modif)::modifs}
| modif=modifierMethod {[modif]}
| modif=modifierClass modifs=modifiersMethod {(modif)::modifs}
| modif=modifierClass {[modif]}
modifierMethod:
| syn=synchronization {Synchronization syn}
| nat=nativity {Nativity nat}

nativity:
| NATIVE {Native}
synchronization:
| SYNCHRONIZED {Synchronized}

returnType:
| VOID {Identifier "void"}
| ret=identifier { ret}

exceptionDecl:
| THROWS exceptions=exceptionsList {Some(exceptions)}
| {None}
exceptionsList:
| exc=except COMMA excList=exceptionsList {(exc)::excList}
| exc=except {[exc]}
except:
| exceptType=identifier { exceptType}
blockOrAbstract:
|blockDecl = blockDeclaration {Some(blockDecl)}

%%
