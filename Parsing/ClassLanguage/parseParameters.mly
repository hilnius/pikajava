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
| param=parameter COMMA paramList = parameterList {(param)::paramList}
| param=parameter {[param]}

parameter:
| paramName = IDENTIFIER {{name=Identifier paramName;param=None;extends=None;super=None}} 
| paramName = IDENTIFIER EXTENDS firstParent=parentParameter {{name= Identifier paramName; param=None;extends=Some(firstParent);super=None}}

(*TODO Correct the parameter AST List Does not extends something but HAS a parameter*)
parentParameter:
| parentName=IDENTIFIER {{name= Identifier parentName;param=None; extends=None; super=None}}
| parentName=IDENTIFIER OPENING_CHEVRON parentParam=parentParameter CLOSING_CHEVRON {{name= Identifier parentName; param=Some(parentParam); extends=None; super=None}}
| parentName=IDENTIFIER OPENING_CHEVRON WILDCARD EXTENDS someParent=parentParameter CLOSING_CHEVRON {{name= Identifier parentName;param=Some({name=Identifier "?";param=None;extends=Some(someParent); super=None}); extends=None; super=None}}  
| parentName=IDENTIFIER OPENING_CHEVRON WILDCARD SUPER someParent=parentParameter CLOSING_CHEVRON {{name= Identifier parentName;param=Some({name=Identifier "?";param=None;extends=None; super=Some(someParent)});extends=None;super=None}}

