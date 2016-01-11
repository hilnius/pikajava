%{
	open Location
	open Located
	open ExitManagement
%}

%start blockDeclaration
%type <Types.block> blockDeclaration
%%

(* if statements *)
ifThenStatement:
| IF OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s=statement { IfStatement(expr, Block([Statement(s)]), Block([])) }
| IF OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s1=statement ELSE s2=statement { IfStatement(expr, Block([Statement(s1)]), Block([Statement(s2)])) }

(* while statements *)
whileStatement:
| WHILE OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s=statement { WhileStatement(expr, Block([Statement(s)])) }

(* for statements *)
forStatement:
| FOR OPENING_PARENTHESIS fi=forInit? SEMICOLON expr=expression? SEMICOLON update=forUpdate? CLOSING_PARENTHESIS s=statement { ForStatement(fi, expr, update, Block([Statement(s)])) }
| FOR OPENING_PARENTHESIS vm=variableModifiers? t=typed i=identifier COLON e=expression CLOSING_PARENTHESIS s=statement { EnhancedForStatement(vm, t, i, e, s) }
forInit:
| s=statementExpressionList               { ForInitStatementExpressionList s }
| s=localVariableDeclaration              { ForInitLocalVariableDeclarationStatement s }
forUpdate:
| s=statementExpressionList               { s }
statementExpressionList:
| s=statementExpression                   { [s] }
| sel=statementExpressionList COMMA s=statementExpression { sel @ [s] }

(* switch statement *)
switchStatement:
| SWITCH OPENING_PARENTHESIS e=expression CLOSING_PARENTHESIS bl=switchBlock { SwitchStatement(e, bl) }
switchBlock:
| OPENING_BRACE labels=switchLabels? CLOSING_BRACE {
    match labels with
    | Some(l2) -> l2
    | None -> []
  }
switchLabels:
| sl=switchLabel                          { [sl] }
| sls=switchLabels sl=switchLabel         { sls @ [sl] }
switchLabel:
| CASE e=expression COLON st=blockStatements? { let a = match st with None -> [] | Some(l) -> l in Case(e, a) }
| DEFAULT COLON                           { Default([]) }

(* try statement *)
tryStatement:
| TRY b=blockDeclaration c=catches                   { TryStatement(b, c, Block([])) }
| TRY b=blockDeclaration c=catches? f=finally        {
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
| CATCH OPENING_PARENTHESIS f=formalParameter CLOSING_PARENTHESIS b=blockDeclaration { CatchClause(f, b) }
finally:
| FINALLY b=blockDeclaration                         { b }
%public formalParameter:
| vdi=variableDeclaratorId                { let (a,b) = vdi in { modifiers=[]; typed=None; declarator=(a,b,None) } }
| vm=variableModifiers t=typed vdi=variableDeclaratorId { let (a,b) = vdi in { modifiers=vm; typed=Some(t); declarator=(a,b,None) } }

(* expressions *)
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
| SYNCHRONIZED OPENING_PARENTHESIS e=expression CLOSING_PARENTHESIS b=blockDeclaration { SynchronizedStatement(e, b) }
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
| s=labeledStatement                      { s }
| s=ifThenStatement                       { s }
| s=whileStatement                        { s }
| s=forStatement                          { s }
| b=blockDeclaration                      { BlockStatement(b) }
| s=emptyStatement                        { s }
| s=expressionStatement                   { s }
| s=assertStatement                       { s }
| s=switchStatement                       { s }
| s=doStatement                           { s }
| s=breakStatement                        { s }
| s=continueStatement                     { s }
| s=returnStatement                       { s }
| s=synchronizedStatement                 { s }
| s=throwStatement                        { s }
| s=tryStatement                          { s }
blockDeclaration:
| OPENING_BRACE b=blockStatements? CLOSING_BRACE {
    match b with
    | None -> Block([])
    | Some(bl) -> Block(bl)
  }
%public blockStatements:
| b=blockStatement                        { [b] }
| bs=blockStatements b=blockStatement     { bs @ [b] }
blockStatement:
| cd=classMemberDeclaration               { ClassDeclarationStatement(cd) } (* TODO : check this *)
| s=statement                             { Statement(s) }

(*| ai=arrayInitializer *)

(* empty parsers *)
labeledStatement:
| i=identifier COLON s=statement          { LabeledStatement(i,s) }
%%
