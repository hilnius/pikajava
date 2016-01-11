%{
open Types
open Location
open ExitManagement
%}

%start  annotations
%type <Types.annotations>  annotations
%%

annotations:
| an=annotation { [an] }
| ans=annotations an=annotation { ans @ [an] }
%public annotation:
| an=normalAnnotation { let (a,b) = an in NormalAnnotation(a,b) }
| an=markerAnnotation { MarkerAnnotation an }
| an=singleElementAnnotation { SingleElementAnnotation an }
normalAnnotation:
| AT tn=typeName OPENING_PARENTHESIS evp=elementValuePairs? CLOSING_PARENTHESIS { (tn, evp) }
elementValuePairs:
| e=elementValuePair { [e] }
| es=elementValuePairs COMMA e=elementValuePair { es @ [e] }
elementValuePair:
| i=identifier EQUAL ev=elementValue { (i, ev) }
%public elementValue:
| ev=conditionalExpression { ConditionalExpression ev }
| ev=annotation { Annotation ev }
| ev=elementValueArrayInitializer { ElementValueArrayInitializer ev }
elementValueArrayInitializer:
| OPENING_BRACE evs=elementValues? COMMA? CLOSING_BRACE { evs }
elementValues:
| ev=elementValue { [ev] }
| evs=elementValues COMMA ev=elementValue { evs @ [ev] }
markerAnnotation:
| AT tn=typeName { tn }
singleElementAnnotation:
| AT tn=typeName OPENING_PARENTHESIS ev=elementValue CLOSING_PARENTHESIS { (tn, ev) }

%%
