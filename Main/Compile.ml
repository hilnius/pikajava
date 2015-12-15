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
