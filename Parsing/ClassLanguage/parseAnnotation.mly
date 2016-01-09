%{
open Types
open Location
open ExitManagement
%}

%start  annotationsList
%type <Types.annotationsList>  annotationsList
%%

(*TODO Parse singler element annotations and more complex annotation*)
annotationsList:
|annotsList=annotations {Some(annotsList)}
| {None}
annotations:
|annot=annotation annots=annotations {(annot)::annots} 
|annot=annotation {[annot]}
annotation:
|markAnnotation=markerAnnotation {markAnnotation}
markerAnnotation:
|AT annotName=IDENTIFIER {annotName}

%%
