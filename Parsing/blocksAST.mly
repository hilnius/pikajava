%{

	type variableDeclaration = Integer of int
	type expression = Bool of bool
	type classDeclaration = Class

	type block = Block of blockStatement list
	and blockStatement =
		  ClassDeclaration of classDeclaration
		| LocalVariableDeclaration of variableDeclaration
		| Statement of statement
	and statement =
		  IfStatement of (expression * block * block)
		| ForStatement of (statement * expression * expression * block)
		| WhileStatement of (expression * block)
		| BlockStatement of block
		| EmptyStatement

%}

%token IF ELSE WHILE FOR OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACKET CLOSE_BRACKET COMMA SEMICOLON EOF

%token <int> INTEGER
%token <bool> BOOLEAN

%start formule
%type <block> formule
%%
formule:
| c=block EOF { c }
(* if statements *)
ifThenStatement:
| IF OPEN_PARENTHESIS expr=expression CLOSE_PARENTHESIS s=statement { IfStatement(expr, Block([s]), Block([])) }
ifThenElseStatement:
| IF OPEN_PARENTHESIS expr=expression CLOSE_PARENTHESIS s1=statementNoShortIf ELSE s2=statement { IfStatement(expr, Block([s1]), Block([s2])) }
ifThenElseStatementNoShortIf:
| IF OPEN_PARENTHESIS expr=expression CLOSE_PARENTHESIS s1=statementNoShortIf ELSE s2=statementNoShortIf { IfStatement(expr, Block([s1]), Block([s2])) }
(* while statements *)
whileStatement:
| WHILE OPEN_PARENTHESIS expr=expression CLOSE_PARENTHESIS s=statement { WhileStatement(expr, Block([s])) }
whileStatementNoShortIf:
| WHILE OPEN_PARENTHESIS expr=expression CLOSE_PARENTHESIS s=statementNoShortIf { WhileStatement(expr, Block([s])) }
(* for statements *)
forStatement:
| s=basicForStatement { s }
(*| s=enhancedForStatement { s }*)
basicForStatement:
| FOR OPEN_PARENTHESIS fi=forInit SEMICOLON expr=expression SEMICOLON update=expression CLOSE_PARENTHESIS s=statement { ForStatement(fi, expr, update, Block([s])) }
forStatementNoShortIf:
| FOR OPEN_PARENTHESIS fi=forInitOpt SEMICOLON expr=expressionOpt SEMICOLON update=forUpdateOpt CLOSE_PARENTHESIS s=statementNoShortIf { ForStatement(fi, expr, update, Block([s])) }
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
statementWithoutTrailingSubstatement:
| b=block { Statement(BlockStatement(b)) }
statementNoShortIf:
| s=statementWithoutTrailingSubstatement { s }
| s=labeledStatementNoShortIf { s }
| s=ifThenElseStatementNoShortIf { Statement(s) }
| s=whileStatementNoShortIf { Statement(s) }
| s=forStatementNoShortIf { Statement(s) }
block:
| OPEN_BRACKET b=blockStatements CLOSE_BRACKET { Block(b) }
blockStatements:
| b=blockStatement { [b] }
| b=blockStatement bs=blockStatements { b::bs }
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
