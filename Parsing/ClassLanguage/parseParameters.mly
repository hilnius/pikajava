%{
open Types
open Location
%}

%start  parametersDeclaration
%type <Types.parameterList>  parametersDeclaration
%%

parametersDeclaration:
| OPENING_CHEVRON paramList=parameterList CLOSING_CHEVRON {Some(paramList)}
| {None}
parameterList:
| param=parameter COMA paramList = parameterList {(param)::paramList}
| param=parameter {[param]}

parameter:
| paramName = IDENTIFIER {{name=Identifier paramName;extends=None;super=None}} 
| paramName = IDENTIFIER EXTENDS firstParent=parentParameter {{name= Identifier paramName; extends=Some(firstParent);super=None}}

(*TODO Correct the parameter AST List Does not extends something but HAS a parameter*)
parentParameter:
| parentName=IDENTIFIER {{name= Identifier parentName; extends=None; super=None}}
| parentName=IDENTIFIER OPENING_CHEVRON WILDCARD EXTENDS someParent=parentParameter CLOSING_CHEVRON {{name= Identifier parentName;extends=Some(someParent); super=None}}  
| parentName=IDENTIFIER OPENING_CHEVRON WILDCARD SUPER someParent=parentParameter CLOSING_CHEVRON {{name= Identifier parentName;extends=None;super=Some(someParent)}}
