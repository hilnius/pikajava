
let rec print_tabs tabs = match tabs with
  | 0 -> ()
  | i -> print_string "    "; print_tabs (i-1)
;;

let print_block tabs b =
  let rec print_block_inner tabs l = match l with
    | [] -> ()
    | h::t -> print_block_statement tabs h; print_block_inner tabs t;
  in
  match b with
    | Block(l) -> print_block_inner tabs l
    | _ -> print_string "error"
;;

let print_local_variable_declaration tabs v = match v with
  | Integer(i) -> begin print_tabs tabs; print_int i; print_newline (); end
    | _ -> print_string "error"
;;

let print_expression expression =
  print_string "expression"
;;

let print_if tabs expression blockIf blockElse =
  begin
  print_tabs tabs;
  print_string "if (";
  print_expression expression;
  print_string ") {";
  print_newline ();

  print_block (tabs + 1) blockIf;

  print_tabs tabs;
  print_string "} else { ";
  print_newline ();

  print_block (tabs + 1) blockElse;

  print_tabs tabs;
  print_string "}";
  print_newline ()
  end
;;

let print_for tabs statement expression1 expression2 block =
  begin
  print_tabs tabs;
  print_string "for (";
  print_newline ();
  print_statement (tabs + 1) statement;
  print_tabs (tabs + 1);
  print_string "; ";
  print_newline ();
  print_tabs (tabs + 1);
  print_expression expression1;
  print_newline ();
  print_tabs (tabs + 1);
  print_string "; ";
  print_newline ();
  print_tabs (tabs + 1);
  print_expression expression2;
  print_newline ();
  print_tabs tabs;
  print_string ") {";
  print_newline ();

  print_block (tabs + 1) block;

  print_tabs tabs;
  print_string "}";
  print_newline ();
  end
;;

let print_while tabs expression block =
  begin
  print_tabs tabs;
  print_string "while (";
  print_expression expression;
  print_string ") {";
  print_newline ();

  print_block (tabs + 1) block;

  print_tabs tabs;
  print_string "}";
  print_newline ();
  end
;;

let print_statement tabs statement = match statement with
  | IfStatement(expression, blockIf, blockElse) -> print_if tabs expression blockIf blockElse
  | ForStatement(statement, expression1, expression2, block) -> print_for tabs statement expression1 expression2 block
  | WhileStatement(expression, block) -> print_while tabs expression block
  | BlockStatement(block) -> print_block tabs block
  | EmptyStatement -> ()
;;

let rec print_block_statement tabs st = match st with
  | LocalVariableDeclaration(lvd) -> print_local_variable_declaration tabs lvd
  | Statement(st) -> print_statement tabs st
  | _ -> ()
;;

let printAST t =
  print_block 0 t
;;
