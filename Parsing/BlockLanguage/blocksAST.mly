%{
	open Location
	open Located
	open BlocksTypes
	open ExitManagement
%}

%start blockDeclaration
%type <BlocksTypes.block> blockDeclaration
%%
blockDeclaration:
| c=block { c }
(* if statements *)
ifThenStatement:
| IF OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s=statement { IfStatement(expr, Block([s]), Block([])) }
ifThenElseStatement:
| IF OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s1=statementNoShortIf ELSE s2=statement { IfStatement(expr, Block([s1]), Block([s2])) }
ifThenElseStatementNoShortIf:
| IF OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s1=statementNoShortIf ELSE s2=statementNoShortIf { IfStatement(expr, Block([s1]), Block([s2])) }
(* while statements *)
whileStatement:
| WHILE OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s=statement { WhileStatement(expr, Block([s])) }
whileStatementNoShortIf:
| WHILE OPENING_PARENTHESIS expr=expression CLOSING_PARENTHESIS s=statementNoShortIf { WhileStatement(expr, Block([s])) }
(* for statements *)
forStatement:
| s=basicForStatement { s }
(*| s=enhancedForStatement { s }*)
basicForStatement:
| FOR OPENING_PARENTHESIS fi=forInit SEMICOLON expr=expression SEMICOLON update=expression CLOSING_PARENTHESIS s=statement { ForStatement(fi, expr, update, Block([s])) }
forStatementNoShortIf:
| FOR OPENING_PARENTHESIS fi=forInitOpt SEMICOLON expr=expressionOpt SEMICOLON update=forUpdateOpt CLOSING_PARENTHESIS s=statementNoShortIf { ForStatement(fi, expr, update, Block([s])) }
forInitOpt:
| f=forInit 		{ f }
|           		{ EmptyStatement }
expressionOpt:
| f=expression 	{ f }
|              	{ Bool(true) }
forUpdateOpt:
| f=forUpdate 	{ f }
|             	{ Bool(true) }
forInit:
| s=statementExpressionList { BlockStatement(Block(s)) }
| s=localVariableDeclarationStatement { BlockStatement(Block([s])) } (* be careful, should be a localVariableDeclaration *)
forUpdate:
| s=statementExpressionList { Bool(true) }
statementExpressionList:
| s=statementExpression { [s] }
| s=statementExpression COMMA sel=statementExpressionList { s::sel }
(* rest *)
statement:
| s=statementWithoutTrailingSubstatement { s }
| s=labeledStatement { s }
| s=ifThenStatement { Statement(s) }
| s=ifThenElseStatement { Statement(s) }
| s=whileStatement { Statement(s) }
| s=forStatement { Statement(s) }
| error { print_string "Error: unable to parse statement "; print_token_full (symbol_loc $startpos $endpos); setExitCodeValue(4); Statement(EmptyStatement)  }
statementWithoutTrailingSubstatement:
| b=block { Statement(BlockStatement(b)) }
statementNoShortIf:
| s=statementWithoutTrailingSubstatement { s }
| s=labeledStatementNoShortIf { s }
| s=ifThenElseStatementNoShortIf { Statement(s) }
| s=whileStatementNoShortIf { Statement(s) }
| s=forStatementNoShortIf { Statement(s) }
block:
| OPENING_BRACKET b=blockStatements CLOSING_BRACKET { Block(b) }
%public blockStatements:
| b=blockStatement { [b] }
| b=blockStatement bs=blockStatements { b::bs }
| 				{ [] }
blockStatement:
| lvds=localVariableDeclarationStatement { lvds }
(* | cd=classDeclaration { cd } *)
| s=statement { s }
expression:
| b=BOOLEAN { Bool(b) }
localVariableDeclarationStatement:
| i=INTEGER { LocalVariableDeclaration(Integer(90)) }
(* empty parsers *)
labeledStatement:
| i=INTEGER { LocalVariableDeclaration(Integer(91)) }
labeledStatementNoShortIf:
| i=INTEGER { LocalVariableDeclaration(Integer(92)) }
statementExpression:
| i=INTEGER { LocalVariableDeclaration(Integer(93)) }
%%
