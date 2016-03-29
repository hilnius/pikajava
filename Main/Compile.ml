open Parser
open Compilation

open AST
open Type

let execute lexbuf verbose =
  try
    print_endline "----------------------------[ \027[96mParsing\027[0m ]----------------------------";
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "\027[32mSuccessfull parsing\027[0m";
    print_endline "";
    print_endline "-----------------------[ \027[96mBuilding registry\027[0m ]-----------------------";
    let registry = ClassRegistry.buildPackageRegistry ast in
    print_endline "\027[32mRegistry successfuly built\027[0m";
    print_endline "\027[33mRegistry is :\027[0m";
    ClassRegistry.printPackage registry;
    print_endline "";
    print_endline "----------------------[ \027[96mBuilding typed AST\027[0m  ]----------------------";
    let typedAST = TypeAST.typeAST ast registry in
    print_endline "\027[32mAST successfuly typed\027[0m";
    print_endline "";
    print_program typedAST;
    print_endline "";
    print_endline "----------------------[ \027[96mChecking typed AST\027[0m  ]----------------------";
    CheckAST.checkAST typedAST registry;
    print_endline "\027[32mAST types checked\027[0m";
    print_endline "";
    print_endline "----------------------[ \027[96mCompiling typed AST\027[0m  ]----------------------";
    let data = Compilation.treeWalk typedAST in
    printData data;
    let test9 = {
      edesc = Val(Int("9"));
      etype = Some(Primitive(Int));
    }
    in
    let scopedData = Execute.executeMethod ((Execute.getMain data.tm),Ref({tpath=[];tid="identifier"})) {data=data; currentScope=0; currentObject={objectId=1;objectName=""; attributes=[]; objectValue=Compilation.Null; scope=0};stack=[]} [Some(test9)] in 
    printData scopedData.data;
    if verbose then AST.print_program typedAST
  with
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l;

