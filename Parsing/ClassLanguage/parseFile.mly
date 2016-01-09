%{
open TypesFile
open Types
open Location
open ExitManagement
%}

%start fileDeclaration
%type <TypesFile.fileTree> fileDeclaration
%%

(*TODO EOF?????*)

(*TODO HANDLE SEVERAL CLASSES OR NONE*)
fileDeclaration:
| pack = package imp = importsListDecl classDecls=classDeclarations EOF {FileTree({pack=pack;imports=imp},classDecls)}
| error {print_string "Error : Invalid Package Declaration\n"; print(symbol_loc $startpos $endpos); setExitCodeValue 1; Empty}
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
classDeclarations:
|classDecls=classDeclarationList {Some classDecls} 
| {None}
classDeclarationList:
|classDecl=classDeclaration classDecls=classDeclarationList {classDecl::classDecls}
|classDecl=classDeclaration {[classDecl]} 
%%	


