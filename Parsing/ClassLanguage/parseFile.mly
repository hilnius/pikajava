%{
open TypesFile
open Types
open Location
%}

%start fileDeclaration
%type <TypesFile.fileTree> fileDeclaration
%%

(*TODO EOF?????*)

(*TODO HANDLE SEVERAL CLASSES OR NONE*)
fileDeclaration:
| pack = package imp = importsListDecl classDecl=classDeclaration {FileTree({pack=pack;imports=imp},classDecl)}
| error {print_string "Error : Invalid Package Declaration\n";print(symbol_loc $startpos $endpos);Empty}
package:
|PACKAGE pack = packageName SEMICOLON {Some(pack)}
| {None}
packageName:
|package = IDENTIFIER DOT childPackage = packageName {package^"."^childPackage}
|package = IDENTIFIER {package}
importsListDecl:
|importsList = importsList {Some importsList}
| {None}
importsList:
|import = imports importsList=importsList {import::importsList}
|import = imports {[import]}
imports:
|IMPORT STATIC import = import {Import(Static,import)}
|IMPORT import = import {Import(NonStatic,import)}
import:
|importName = IDENTIFIER childName=importNext SEMICOLON {importName^childName}
importNext:
|DOT importName = IDENTIFIER childName=importNext {"."^importName^childName}
|DOT STAR {".*"}
| {""}
%%	


