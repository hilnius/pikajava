%{
open Types
open Location
open ExitManagement
%}

%start  typeParameters
%type <Types.typeParameterList>  typeParameters
%%

typeParameters:
| OPENING_CHEVRON paramList=typeParameterList CLOSING_CHEVRON { paramList }
typeParameterList:
| tp=typeParameter { [tp] }
| tpl=typeParameterList COMMA tp=typeParameter { tpl @ [tp] }

typeParameter:
| tv=typeVariable tb=typeBound? { (tv, tb)  }
typeBound:
| EXTENDS coif=classOrInterfaceType abl=additionalBoundList? { (coif, abl) }
additionalBoundList:
| ab=additionalBound abl=additionalBoundList { ab::abl }
| ab=additionalBound { [ab] }
additionalBound:
| BITAND it=interfaceType { it }

