%{
open Types
open Location
open ExitManagement
%}

%start fileDeclaration
%type <Types.compilationUnit> fileDeclaration
%%

(*TODO EOF?????*)

(*TODO HANDLE SEVERAL CLASSES OR NONE*)
fileDeclaration:
| c=compilationUnit EOF { c }

compilationUnit:
| pd=packageDeclaration? id=importDeclarations? td=typeDeclarations? { (pd,id,td) }
importDeclarations:
| id=importDeclaration { [id] }
| ids=importDeclarations id=importDeclaration { ids @ [id] }
typeDeclarations:
| td=typeDeclaration { [td] }
| tds=typeDeclarations td=typeDeclaration { tds @ [td] }

packageDeclaration:
| an=annotations? PACKAGE p=packageName { (an, p) }

importDeclaration:
| id=singleTypeImportDeclaration { SingleImportDeclaration id }
| id=typeImportOnDemandDeclaration { TypeImportOnDemandDeclaration id }
| id=singleStaticImportDeclaration { let (a,b) = id in SingleStaticImportDeclaration(a,b) }
| id=staticImportOnDemandDeclaration { StaticImportOnDemandDeclaration id }
singleTypeImportDeclaration:
| IMPORT tn=typeName SEMICOLON { tn }
typeImportOnDemandDeclaration:
| IMPORT p=packageOrTypeName DOT ASTERISK SEMICOLON { p }
singleStaticImportDeclaration:
| IMPORT STATIC t=typeName DOT i=identifier SEMICOLON { (t,i) }
staticImportOnDemandDeclaration:
| IMPORT STATIC t=typeName DOT ASTERISK SEMICOLON { t }

typeDeclaration:
| c=classDeclaration { c }
(*| i=interfaceDeclaration { i } *)
| SEMICOLON { EmptyContent }

%%


