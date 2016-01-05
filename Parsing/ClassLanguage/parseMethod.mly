%{
open Types
open Location
%}

%start  methodDeclaration
%type <Types.methodTree>  methodDeclaration
%%

methodDeclaration :
| modifs=modifiersMethod params=parametersDeclaration return=IDENTIFIER methodName=IDENTIFIER OPENING_PARENTHESIS arguments = arguments CLOSING_PARENTHESIS exceptions=exceptionDecl OPENING_BRACKET CLOSING_BRACKET
	{MethodTree({parameters=params; modif=Some(modifs); returnType= Identifier return; name= Identifier methodName; args=arguments; thr=exceptions; con=None});}
| error {print_string "Error : Invalid Method Declaration\n";print(symbol_loc $startpos $endpos);Empty}	
modifiersMethod:
| modif=modifierMethod modifs=modifiersMethod {(modif)::modifs}
| modif=modifierMethod {[modif]}
modifierMethod:
| vis=visibility {Visibility vis}
| abs=abstraction {Abstraction abs}
| fin=finality {Finality fin}
| sta=staticity {Staticity sta}
| syn=synchronization {Synchronization syn}
| strict=strictfp {StrictFpity strict}
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

arguments:
| argList = argumentsList {Some(argList)}
| {None}
argumentsList:
| arg=argument COMA argList=argumentsList {arg::argList}
| arg=argument {[arg]}
argument:
| argType=IDENTIFIER argName=IDENTIFIER {{argType=Identifier argType; argName=Identifier argName}}

exceptionDecl:
| THROWS exceptions=exceptionsList {Some(exceptions)}
| {None}
exceptionsList:
| exc=except COMA excList=exceptionsList {(exc)::excList}
| exc=except {[exc]}
except:
| exceptType=IDENTIFIER {Identifier exceptType}

%%	
