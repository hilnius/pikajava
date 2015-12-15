%{
open TypesFile
open Location
%}


%token EOF
%token PACKAGE
%token IMPORT STATIC
%token<string> IMPORT_OR_PACKAGE_NAME
%token SEMICOLON DOT STAR

%start fileDeclaration
%type <TypesFile.fileTree> fileDeclaration
%%

(*TODO EOF?????*)
fileDeclaration:
| pack = package SEMICOLON imp = importsListDecl EOF {FileTree({pack=pack;imports=imp},Empty)}
| error {print_string "Error : Invalid Class Declaration\n";print(symbol_loc $startpos $endpos);Empty}
package:
|PACKAGE pack = packageName {Some(pack)}
| {None}
packageName:
|package = IMPORT_OR_PACKAGE_NAME DOT childPackage = packageName {package^"."^childPackage}
|package = IMPORT_OR_PACKAGE_NAME {package}
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
|importName = IMPORT_OR_PACKAGE_NAME childName=importNext SEMICOLON {importName^childName}
importNext:
|DOT importName = IMPORT_OR_PACKAGE_NAME childName=importNext {"."^importName^childName}
|DOT STAR {".*"}
| {""}
%%	


