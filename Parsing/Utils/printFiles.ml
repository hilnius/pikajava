open TypesFile

let printStaticness staticness = match staticness with
|Static -> print_string "static"
|NonStatic -> print_string "NonStatic" 

let printImport import = match import with 
|Import(staticness,importName)->printStaticness staticness; print_string (" "^importName^"\n")

  

let rec printImports imports = match imports with
|Some(a::t) -> print_string "Import : " ; printImport a; printImports (Some(t));
|Some([]) -> print_string "End Imports\n"
|None -> print_string("No Import\n")

let printPackage packageName = match packageName with
|Some (name) -> print_string ("package : "^name^"\n");
|None ->  print_string "no package\n"

let printFileTree tree = match tree with
| FileTree({pack=pack; imports=imports;}, Empty) ->
	printPackage pack; printImports imports;
