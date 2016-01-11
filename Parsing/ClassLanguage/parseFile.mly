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
| error {print_string "\027[31mError: unable to parse file "; print_token_full (symbol_loc $startpos $endpos); setExitCodeValue 2; print_string "\027[0m"; raise (SyntaxError "Cannot parse file") }

compilationUnit:
| td=typeDeclarations? { (None,None,td) }
| id=importDeclarations td=typeDeclarations? { (None,Some(id),td) }
| td=typeDeclarations? { (None,None,td) }
| pd=classMemberDeclaration id=importDeclarations td=typeDeclarations? { (Some(pd),Some(id),td) }
importDeclarations:
| id=importDeclaration { [id] }
| ids=importDeclarations id=importDeclaration { ids @ [id] }
typeDeclarations:
| td=classMemberDeclaration { [td] }
| tds=typeDeclarations td=classMemberDeclaration { tds @ [td] }

%public unmodifiedPackageDeclaration:
| PACKAGE p=packageName SEMICOLON { PackageDeclaration(p) }

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


%%


