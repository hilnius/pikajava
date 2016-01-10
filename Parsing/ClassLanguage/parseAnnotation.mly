%{
open Types
open Location
open ExitManagement
%}

%start  annotation
%type <Types.annotation>  annotation
%%

(*TODO Parse singler element annotations and more complex annotation*)
annotation:
|markAnnotation=markerAnnotation {markAnnotation}
markerAnnotation:
|AT annotName=IDENTIFIER {annotName}

%%
