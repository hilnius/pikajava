%{
open Types
open Location
open ExitManagement
%}

%start  methodDeclaration
%type <Types.classContentTree>  methodDeclaration
%%

methodDeclaration :
| annotations=annotationsList modifs=modifiersMethodList params=parametersDeclaration return=returnType methodName=IDENTIFIER OPENING_PARENTHESIS arguments = arguments CLOSING_PARENTHESIS exceptions=exceptionDecl block=blockOrAbstract
	{MethodTree({parameters=params; annots=annotations; modif=modifs; returnType= return; name= Identifier methodName; args=arguments; thr=exceptions; con=block});}	
(*| error {print_string "Error : Invalid Method Declaration\n";print(symbol_loc $startpos $endpos); setExitCodeValue 3; Empty}*)	

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
(*visibilit:
|PUBLIC {Public}
|PRIVATE {Private}
|PROTECTED {Protected}
abstractio:
|ABSTRACT {Abstract}
finalit:
|FINAL {Final}
staticity:
| STATIC {Static}
strictfp:
| STRICTFP {StrictFp}*)
nativity:
| NATIVE {Native}
synchronization:
| SYNCHRONIZED {Synchronized}

returnType:
| VOID {Identifier "void"}
| ret=IDENTIFIER {Identifier ret} 
arguments:
| argList = argumentsList {Some(argList)}
| {None}
argumentsList:
| arg=argument COMMA argList=argumentsList {arg::argList}
| arg=argument {[arg]}
argument:
| argType=IDENTIFIER argName=IDENTIFIER {{argType=Identifier argType; argName=Identifier argName}}

exceptionDecl:
| THROWS exceptions=exceptionsList {Some(exceptions)}
| {None}
exceptionsList:
| exc=except COMMA excList=exceptionsList {(exc)::excList}
| exc=except {[exc]}
except:
| exceptType=IDENTIFIER {Identifier exceptType}
blockOrAbstract:
|blockDecl = blockDeclaration {Some(blockDecl)}
|SEMICOLON {None}

%%	
