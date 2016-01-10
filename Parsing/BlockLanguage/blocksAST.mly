%{
	open Location
	open Located
	open ExitManagement
%}

%start blockDeclaration
%type <Types.block> blockDeclaration
%%
blockDeclaration:
| c=block { c }

(* if statements *)
ifThenStatement:
| IF OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s=statement { IfStatement(expr, Block([Statement(s)]), Block([])) }
ifThenElseStatement:
| IF OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s1=statementNoShortIf ELSE s2=statement { IfStatement(expr, Block([Statement(s1)]), Block([Statement(s2)])) }
ifThenElseStatementNoShortIf:
| IF OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s1=statementNoShortIf ELSE s2=statementNoShortIf { IfStatement(expr, Block([Statement(s1)]), Block([Statement(s2)])) }

(* while statements *)
whileStatement:
| WHILE OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s=statement { WhileStatement(expr, Block([Statement(s)])) }
whileStatementNoShortIf:
| WHILE OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s=statementNoShortIf { WhileStatement(expr, Block([Statement(s)])) }

(* for statements *)
forStatement:
| s=basicForStatement                     { s }

(*| s=enhancedForStatement { s }*)
basicForStatement:
| FOR OPENING_PARENTHESIS fi=forInit SEMICOLON expr=expression SEMICOLON update=expression CLOSING_PARENTHESIS s=statement { ForStatement(fi, expr, update, Block([Statement(s)])) }
forStatementNoShortIf:
| FOR OPENING_PARENTHESIS fi=forInitOpt SEMICOLON expr=expressionOpt SEMICOLON update=forUpdateOpt CLOSING_PARENTHESIS s=statementNoShortIf { ForStatement(fi, expr, update, Block([Statement(s)])) }
forInitOpt:
| f=forInit 		                          { f }
|           		                          { EmptyStatement }
expressionOpt:
| f=expression 	                          { f }
|              	                          { Bool(true) }
forUpdateOpt:
| f=forUpdate 	                          { f }
|             	                          { Bool(true) }
forInit:
| s=statementExpressionList               { BlockStatement(Block(s)) }
| s=localVariableDeclarationStatement     { BlockStatement(Block([s])) } (* be careful, should be a localVariableDeclaration *)
forUpdate:
| s=statementExpressionList               { Bool(true) }
statementExpressionList:
| s=statementExpression                   { [s] }
| s=statementExpression COMMA sel=statementExpressionList { s::sel }

(* switch statement *)
switchStatement:
| SWITCH OPENING_PARENTHESIS e=expression CLOSING_PARENTHESIS bl=switchBlock { SwitchStatement(e, bl) }
switchBlock:
| OPENING_BRACKET gr=switchBlockStatementGroups? labels=switchLabels? CLOSING_BRACKET {
    match gr,labels with
    | Some(l1), Some(l2) -> l1 @ l2
    | Some(l1), None -> l1
    | None, Some(l2) -> l2
    | None, None -> []
  }
switchBlockStatementGroups:
| s=switchBlockStatementGroup             { s }
| gr=switchBlockStatementGroups s=switchBlockStatementGroup { gr @ s }
switchBlockStatementGroup:
| labels=switchLabels st=blockStatements  {
    let rec buildWithLast labels st =
      match labels with
      | [Case(e,_)] -> [Case(e, st)]
      | [Default(_)] -> [Default(st)]
      | t::q -> t::(buildWithLast q st)
    in
      buildWithLast labels st
  }
switchLabels:
| sl=switchLabel                          { [sl] }
| sls=switchLabels sl=switchLabel         { sls @ [sl] }
switchLabel:
| CASE e=constantExpression COLON         { Case(e, []) }
| CASE e=enumConstantName COLON           { Case(e, []) }
| DEFAULT COLON                           { Default([]) }
enumConstantName:
| i=identifier                            { i }

(* try statement *)
tryStatement:
| TRY b=block c=catches                   { TryStatement(b, c, Block([])) }
| TRY b=block c=catches? f=finally        {
    let catches = match c with
      | None -> []
      | Some(ca) -> ca
    in
      TryStatement(b, catches, f)
  }
catches:
| c=catchClause                           { [c] }
| cs=catches c=catchClause                { cs @ [c] }
catchClause:
| CATCH OPENING_PARENTHESIS f=formalParameter CLOSING_PARENTHESIS b=block { CatchClause(f, b) }
finally:
| FINALLY b=block                         { b }
formalParameter:
| vdi=variableDeclaratorId                { vdi }
(* real here : | variableModifiers type variableDeclaratorId *)
variableDeclaratorId:
| i=identifier                            { i }
(*| variableDeclaratorId [ ] *)

(* small statements *)
emptyStatement:
| SEMICOLON                               { EmptyStatement }
assertStatement:
| ASSERT e1=expression SEMICOLON          { AssertStatement(e1, None) }
| ASSERT e1=expression COLON e2=expression SEMICOLON { AssertStatement(e1, Some(e2)) }
breakStatement:
| BREAK i=identifier? SEMICOLON           { BreakStatement(i) }
continueStatement:
| CONTINUE i=identifier? SEMICOLON        { ContinueStatement(i) }
returnStatement:
| RETURN e=expression? SEMICOLON          { ReturnStatement(e) }
throwStatement:
| THROW e=expression SEMICOLON            { ThrowStatement(e) }
synchronizedStatement:
| SYNCHRONIZED OPENING_PARENTHESIS e=expression CLOSING_PARENTHESIS b=block { SynchronizedStatement(e, b) }
doStatement:
| DO s=statement WHILE OPENING_PARENTHESIS e=expression CLOSING_PARENTHESIS SEMICOLON { DoWhileStatement(e, s) }

(* blocks *)
statement:
| s=statementWithoutTrailingSubstatement  { s }
| s=labeledStatement                      { s }
| s=ifThenStatement                       { s }
| s=ifThenElseStatement                   { s }
| s=whileStatement                        { s }
| s=forStatement                          { s }
statementWithoutTrailingSubstatement:
| b=block                                 { BlockStatement(b) }
| s=emptyStatement                        { s }
(*| s=expressionStatement                   { }*)
| s=assertStatement                       { s }
| s=switchStatement                       { s }
| s=doStatement                           { s }
| s=breakStatement                        { s }
| s=continueStatement                     { s }
| s=returnStatement                       { s }
| s=synchronizedStatement                 { s }
| s=throwStatement                        { s }
| s=tryStatement                          { s }
statementNoShortIf:
| s=statementWithoutTrailingSubstatement  { s }
| s=labeledStatementNoShortIf             { s }
| s=ifThenElseStatementNoShortIf          { s }
| s=whileStatementNoShortIf               { s }
| s=forStatementNoShortIf                 { s }
block:
| OPENING_BRACKET b=blockStatements CLOSING_BRACKET { Block(b) }
%public blockStatements:
| b=blockStatement                        { [b] }
| bs=blockStatements b=blockStatement     { bs @ [b] }
blockStatement:
| lvds=localVariableDeclarationStatement  { lvds }
| cd=classDeclaration                     { ClassDeclarationStatement(cd) }
| s=statement                             { Statement(s) }

(* expressions *)
expression:
| b=BOOLEAN                               { Bool(b) }
localVariableDeclarationStatement:
| i=INTEGER                               { LocalVariableDeclaration(Integer(90)) }
(* empty parsers *)
labeledStatement:
| i=INTEGER                               { BlockStatement(Block([LocalVariableDeclaration(Integer(91))])) }
labeledStatementNoShortIf:
| i=INTEGER                               { BlockStatement(Block([LocalVariableDeclaration(Integer(92))])) }
statementExpression:
| i=INTEGER                               { LocalVariableDeclaration(Integer(93)) }
identifier:
| b=BOOLEAN                               { Bool(b) }
constantExpression:
| b=BOOLEAN                               { Bool(b) }
%%
