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
| s=enhancedForStatement                  { s }
basicForStatement:
| FOR OPENING_PARENTHESIS fi=forInit? SEMICOLON expr=expression? SEMICOLON update=forUpdate? CLOSING_PARENTHESIS s=statement { ForStatement(fi, expr, update, Block([Statement(s)])) }
enhancedForStatement:
| FOR OPENING_PARENTHESIS vm=variableModifiers? t=typed i=identifier COLON e=expression CLOSING_PARENTHESIS s=statement { EnhancedForStatement(vm, t, i, e, s) }
forStatementNoShortIf:
| FOR OPENING_PARENTHESIS fi=forInit? SEMICOLON expr=expression? SEMICOLON update=forUpdate? CLOSING_PARENTHESIS s=statementNoShortIf { ForStatement(fi, expr, update, Block([Statement(s)])) }
forInit:
| s=statementExpressionList               { ForInitStatementExpressionList s }
| s=localVariableDeclarationStatement     { ForInitLocalVariableDeclarationStatement s }
forUpdate:
| s=statementExpressionList               { s }
statementExpressionList:
| s=statementExpression                   { [s] }
| s=statementExpression COMMA sel=statementExpressionList { s::sel }

(* switch statement *)
switchStatement:
| SWITCH OPENING_PARENTHESIS e=expression CLOSING_PARENTHESIS bl=switchBlock { SwitchStatement(e, bl) }
switchBlock:
| OPENING_BRACE gr=switchBlockStatementGroups? labels=switchLabels? CLOSING_BRACE {
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
| i=expression                            { i }

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
%public formalParameter:
| vdi=variableDeclaratorId                { let (a,b) = vdi in { modifiers=[]; typed=None; declarator=(a,b,None) } }
| vm=variableModifiers t=typed vdi=variableDeclaratorId { let (a,b) = vdi in { modifiers=vm; typed=Some(t); declarator=(a,b,None) } }

(* expressions *)
localVariableDeclarationStatement:
| lvd=localVariableDeclaration SEMICOLON  { LocalVariableDeclarationStatement(lvd) }

localVariableDeclaration:
| vm=variableModifiers? t=typed vd=variableDeclarators { match vm with None -> ([], t, vd) | Some(v) -> (v, t, vd)  }

%public variableDeclarators:
| vd=variableDeclarator                   { [vd] }
| vds=variableDeclarators COMMA vd=variableDeclarator { vds @ [vd] }

variableDeclarator:
| vdi=variableDeclaratorId                { let (a,b) = vdi in (a,b,None) }
| vdi=variableDeclaratorId EQUAL vi=variableInitializer { let (a,b) = vdi in (a,b,Some(vi)) }
%public variableDeclaratorId:
| id=identifier                           { (id,0) }
| vdi=variableDeclaratorId BRACKETOPEN BRACKETCLOSE { let (a,b) = vdi in (a,b + 1) }
variableInitializer:
| e=expression                            { VariableInitializerExpression(e) }

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
expressionStatement:
| s=statementExpression SEMICOLON { ExpressionStatement(s) }
statementExpression:
| e=assignment                            { AssignmentStatement e }
| e=preIncrementExpression                { PreIncrementExpressionStatement e }
| e=preDecrementExpression                { PreDecrementExpressionStatement e }
| e=postIncrementExpression               { PostIncrementExpressionStatement e }
| e=postDecrementExpression               { PostDecrementExpressionStatement e }
| e=methodInvocation                      { MethodInvocationStatement e }
| e=classInstanceCreationExpression       { ClassInstanceCreationExpressionStatement e }
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
(*| s=expressionStatement                   { s } *)
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
| OPENING_BRACE b=blockStatements? CLOSING_BRACE {
    match b with
    | None -> Block([])
    | Some(bl) -> Block(bl)
  }
%public blockStatements:
| b=blockStatement                        { [b] }
| bs=blockStatements b=blockStatement     { bs @ [b] }
blockStatement:
| lvds=localVariableDeclarationStatement  { lvds }
| cd=objectDeclaration                    { ClassDeclarationStatement(cd) } (* TODO : check this *)
| s=statement                             { Statement(s) }

(*| ai=arrayInitializer *)

(* empty parsers *)
labeledStatement:
| i=identifier COLON s=statement          { LabeledStatement(i,s) }
labeledStatementNoShortIf:
| i=identifier COLON s=statementNoShortIf { LabeledStatement(i,s) }
constantExpression:
| b=expression                            { b }
%%
