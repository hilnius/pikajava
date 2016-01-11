open ExitManagement
(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose =
  print_string "Computing AST";
  print_newline ();
  (* Compile for Robin
  let ast = (BlocksAST.formule BlocksLexer.nexttoken lexbuf) in
  begin
    print_string "Printing AST";
    print_newline ();
    (* PrintAST.printAST(ast); *)
    print_string "AST is printed";
    print_newline ();
  end *)
  (*PrintTypes.printTree (ParseClass.classDeclaration LexClass.nextToken lexbuf)*)

 (*LexFile.examineAll lexbuf;*)

  Parsers.fileDeclaration LexFile.nextToken lexbuf;
 (*PrintFiles.printFileTree(Parsers.fileDeclaration LexFile.nextToken lexbuf); *)

  (* let a = Parsers.declaration LexFile.nextToken lexbuf in
  match a with
  | MethodDeclaration(_) -> print_string "parsed method\n"
  | ConstructorDeclaration(_) -> print_string "parsed constructor\n"; *)

  exit !exitCodeValue

