open Types

(*functions to print the different modifiers *)
let printVisibility vis = match vis with
|Public -> print_string "visibility:public\n"
|Private -> print_string "visibility:private\n"
|Protected -> print_string "visibility:protected\n"
|Private_Package -> print_string "visibility:private package\n"

let printObjectType obj = match obj with
|Interface -> print_string "Type : Interface\n"
|Class -> print_string "Type : Class\n"
|Enum -> print_string "Type : Enum\n"

let printAbstraction abs= match abs with
|Abstract -> print_string "abstraction:abstract\n"
|Concrete -> print_string "abstraction:concrete\n"

let printFinality fin = match fin with
|Final -> print_string "finality:final\n"
|Extendable -> print_string "finality:extendable\n"

let printStaticity sta = match sta with
|Static -> print_string "staticity:static\n"
|NonStatic -> print_string "staticity:Non static\n"

(*function to print the class name *)
let printIdentifier iden = match iden with
|Identifier(identName) -> print_string (identName^"\n")


(*function to print the class parent name*)


let rec printExceptions exceptions = match exceptions with
Some(a::t) -> print_string "exception : " ; printIdentifier a; printExceptions (Some(t));
|Some([]) -> print_string "End exceptions\n"
|None -> print_string("No exception\n")


let printArgument argument = match argument with
| {argType=argType; argName=argName}->printIdentifier argType; printIdentifier argName

let rec printArguments arguments = match arguments with
Some(a::t) -> print_string "argument : " ; printArgument a; printArguments (Some(t));
|Some([]) -> print_string "End arguments\n"
|None -> print_string("No argument\n")

let printAnnotation annotation =  match annotation with
| annotName -> print_string (annotName^"\n")

let rec printAnnotations annotations = match annotations with
Some(a::t) -> print_string "annotation : " ; printAnnotation a; printAnnotations (Some(t));
|Some([]) -> print_string "End annotations\n"
|None -> print_string("No annotation\n")

let printModifier modifier = match modifier with
|Visibility vis -> printVisibility vis
|Abstraction abs -> printAbstraction abs
|Finality fin -> printFinality fin
|Synchronization syn  -> print_string "Synchronized\n"
|Nativity nat -> print_string "Native\n"
|StrictFpity str -> print_string "StrictFp\n"
|Staticity sta -> print_string "Static\n"

let rec printModifiers modifiers = match modifiers with
Some(a::t) -> print_string "modifier : " ; printModifier a; printModifiers (Some(t));
|Some([]) -> print_string "End modifiers\n"
|None -> print_string("No modifier\n")



let rec printParameter param = match param with
|{name=paramName;param=Some(parentParameter);extends=Some(parentName);super=None} -> printIdentifier paramName; print_string "parentParameter:";
printParameter parentParameter;print_string "extends:";printParameter parentName;
|{name=paramName;param=Some(parentParameter);extends=None;super=Some(childName)} -> printIdentifier paramName;print_string "parentParameter:";printParameter parentParameter; print_string "super:";printParameter childName;
|{name=paramName;param=Some(parentParameter);extends=None;super=None} -> printIdentifier paramName ;print_string "parentParameter:";printParameter parentParameter;

|{name=paramName;param=None;extends=Some(parentName);super=None} -> printIdentifier paramName; print_string "parentParameter:none\n";print_string "extends:";printParameter parentName;
|{name=paramName;param=None;extends=None;super=Some(childName)} -> printIdentifier paramName;print_string "parentParameter: none\n";print_string "super:";printParameter childName;
|{name=paramName;param=None;extends=None;super=None} -> printIdentifier paramName ;print_string "parentParameter: none\n"



(*function to print the parameters of the class or interface*)
let rec printParameters params = match params with
Some(a::t) -> print_string "param : " ; printParameter a; printParameters (Some(t));
|Some([]) -> print_string "End params\n"
|None -> print_string("No params\n")

let printExtendsParent parent = match parent with
|Some(Parent({name=parentName;parameters=parameters})) -> print_string ("class parent:"^parentName^"\n"); printParameters parameters
|None -> print_string("No parents\n")
let printImplementsParent parent= match parent with
|Parent({name=parentName;parameters=parameters}) -> print_string ("interface parent:"^parentName^"\n"); printParameters parameters

let rec printParents parents = match parents with
Some(a::t) -> print_string "parent : " ; printImplementsParent a; printParents (Some(t));
|Some([]) -> print_string "End parents\n"
|None -> print_string("No parents\n")

let rec printInterfaces interfaces = match interfaces with
|Some(a::t) -> print_string "Interface : " ; printIdentifier a; printInterfaces (Some(t));
|Some([]) -> print_string "End Interfaces\n"
|None -> print_string("No interface\n")

let rec printMethodContent aMethod = match aMethod with
| Some(b) -> printAST b
| None -> print_string ("Abstract Method\n")


and printCon content = match content with
Some(a::t) -> print_string "classContent : " ; printClassContentTree a; printCon (Some(t));
|Some([]) -> print_string "End classContent\n"
|None -> print_string("No classContent\n")
and printTree tree = match tree with
| ClassTree({objectType=obj; annots=annotations; modif=modifiersObject; inh=parent; impl=interfaces; parameters=params; className=identifier; con=content}) ->
	printObjectType obj; printAnnotations annotations; printModifiers modifiersObject; printParameters params; printExtendsParent parent; printParents interfaces; printIdentifier identifier; printCon content
| InterfaceTree({objectType= obj; annots=annotations; modif=modifiersObject;interfaceName=interfaceName;parameters=params;inh=parent;con=content}) ->
	printObjectType obj; printAnnotations annotations; printModifiers modifiersObject; printIdentifier interfaceName; printParameters params; printParents parent; printCon content
| EnumTree	({objectType=obj; annots=annotations; modif=modifiersObject;enumName=enumName;inh=parent;con=content}) ->
	printObjectType obj; printAnnotations annotations; printModifiers modifiersObject; printIdentifier enumName; printParents parent; printCon content
| ErrorDecl error -> print_string error

and printClassContentTree tree = match tree with
| Initializer ({iniType=iniType;con=block}) -> print_string "Initializer : "; printStaticity iniType; printAST block
| MethodTree ({parameters=parameterList;  annots=annotations; modif=modifiersMethod; returnType=returnType; name=methodName; args=arguments; thr=exceptionList; con=block }) ->
	printParameters parameterList; printAnnotations annotations; printModifiers modifiersMethod; printIdentifier returnType; printIdentifier methodName; printArguments arguments; printExceptions exceptionList;
	printMethodContent block
| ObjectTree objectTree -> printTree objectTree;
| ErrorDecl error -> print_string error


and print_tabs tabs = match tabs with
  | 0 -> ()
  | i -> print_string "    "; print_tabs (i-1)

and print_block tabs b =
  let rec print_block_inner tabs l = match l with
    | [] -> ()
    | h::t -> print_block_statement tabs h; print_block_inner tabs t;
  in
  match b with
    | Block(l) -> print_block_inner tabs l
    | _ -> print_string "error"

and print_local_variable_declaration tabs v = match v with
  | Integer(i) -> begin print_tabs tabs; print_int i; print_newline (); end
  | _ -> print_string "error"

and print_expression expression =
  match expression with
  | Bool(true) -> print_string "true"
  | Bool(false) -> print_string "false"
  | _ -> print_string "something"

and print_optional_expression prefix e =
  match e with
  | None -> ()
  | Some(v2) -> begin
      print_string prefix;
      print_expression v2;
    end

and print_if tabs expression blockIf blockElse =
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

and print_assert tabs e1 e2 =
  begin
  print_tabs tabs;
  print_string "assert ";
  print_expression e1;
  print_optional_expression " : " e2;
  print_string ";\n";
  end

and print_for tabs statement expression1 expression2 block =
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

and print_switch tabs e l =
  let rec print_switch_statements tabs st = match st with
    | [] -> ()
    | Case(e,b)::q -> begin
        print_tabs tabs;
        print_string "case ";
        print_expression e;
        print_string ":\n";
        print_block (tabs + 1) (Block(b));
        print_switch_statements tabs q;
      end
    | Default(b)::q -> begin
        print_tabs tabs;
        print_string "default:\n";
        print_block (tabs + 1) (Block(b));
        print_switch_statements tabs q;
      end
  in
  begin
  print_tabs tabs;
  print_string "switch (";
  print_expression e;
  print_string ") {";
  print_newline ();

  print_switch_statements (tabs + 1) l;

  print_string "}";
  print_newline ();
  end

and print_do_while tabs e s =
  begin
  print_tabs tabs;
  print_string "do {\n";
  print_statement (tabs + 1) s;
  print_tabs tabs;
  print_string "} while (";
  print_expression e;
  print_string ");\n";
  end

and print_while tabs expression block =
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

and print_try tabs tryBlock catches finallyBlock =
  let rec print_catches l = match l with
    | [] -> ()
    | CatchClause(a,b)::q -> begin
      print_string "catch (";
      print_expression a;
      print_string ") {\n";
      print_block (tabs+1) b;
      print_string "} ";
      print_catches q;
    end
  in
  begin
  print_tabs tabs;
  print_string "try {\n";
  print_block (tabs + 1) tryBlock;
  print_tabs tabs;
  print_string "} ";
  print_catches catches;
  print_string "finally {\n";
  print_block (tabs + 1) finallyBlock;
  print_string "}\n";
  end

and print_synchronized tabs expression block =
  begin
  print_tabs tabs;
  print_string "synchronized (";
  print_expression expression;
  print_string ") {";
  print_newline ();

  print_block (tabs + 1) block;

  print_tabs tabs;
  print_string "}";
  print_newline ();
  end

and print_break tabs expression =
  begin
  print_tabs tabs;
  print_string "break";
  print_optional_expression " " expression;
  print_string ";\n";
  end

and print_continue tabs expression =
  begin
  print_tabs tabs;
  print_string "continue";
  print_optional_expression " " expression;
  print_string ";\n";
  end

and print_return tabs expression =
  begin
  print_tabs tabs;
  print_string "return";
  print_optional_expression " " expression;
  print_string ";\n";
  end

and print_throw tabs expression =
  begin
  print_tabs tabs;
  print_string "throw ";
  print_expression expression;
  print_string ";\n";
  end

and print_statement tabs statement = match statement with
  | IfStatement(expression, blockIf, blockElse) -> print_if tabs expression blockIf blockElse
  | ForStatement(statement, expression1, expression2, block) -> print_for tabs statement expression1 expression2 block
  | WhileStatement(expression, block) -> print_while tabs expression block
  | BlockStatement(block) -> print_block tabs block
  | AssertStatement(e1, e2) -> print_assert tabs e1 e2
  | SwitchStatement(e, statements) -> print_switch tabs e statements
  | DoWhileStatement(e, statement) -> print_do_while tabs e statement
  | BreakStatement(e) -> print_break tabs e
  | ContinueStatement(e) -> print_continue tabs e
  | ReturnStatement(e) -> print_return tabs e
  | ThrowStatement(e) -> print_throw tabs e
  | SynchronizedStatement(e, block) -> print_synchronized tabs e block
  | TryStatement(tryBlock, catches, finallyBlock) -> print_try tabs tryBlock catches finallyBlock
  | EmptyStatement -> ()

and print_block_statement tabs st = match st with
  | LocalVariableDeclaration(lvd) -> print_local_variable_declaration tabs lvd
  | ClassDeclarationStatement(cl) -> printTree cl
  | Statement(st) -> print_statement tabs st
  | _ -> ()

and printAST t =
  print_block 0 t
;;



