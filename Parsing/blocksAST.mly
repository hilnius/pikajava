%{

	 (*type statementExpression =
		  Assignment of assignment
		 | PreIncrementExpression of preIncrementExpression
		 | PreDecrementExpression of preDecrementExpression
		 | PostIncrementExpression of postIncrementExpression
		 | PostDecrementExpression of postDecrementExpression
		 | MethodInvocation of methodInvocation
		 | ClassInstanceCreationExpression of classInstanceCreationExpression

	 type expressionStatement = StatementExpression of statementExpression

	type statementWithoutTrailingSubstatement =
		  Block of block
		 | EmptyStatement of emptyStatement
		 | ExpressionStatement of expressionStatement
		 | AssertStatement of assertStatement
		 | SwitchStatement of switchStatement
		 | DoStatement of doStatement
		 | BreakStatement of breakStatement
		 | ContinueStatement of continueStatement
		 | ReturnStatement of returnStatement
		 | SynchronizedStatement of synchronizedStatement
		 | ThrowStatement of throwStatement
		 | TryStatement of tryStatement

	type statementNoShortIf =
		  StatementWithoutTrailingSubstatement of statementWithoutTrailingSubstatement
		 | LabeledStatementNoShortIf of labeledStatementNoShortIf
		| IfThenElseStatementNoShortIf of ifThenElseStatementNoShortIf
		 | WhileStatementNoShortIf of whileStatementNoShortIf
		 | ForStatementNoShortIf of forStatementNoShortIf

	 type ifThenStatement = IfThenStatement of (expression * statement)
	type ifThenElseStatement = IfThenElseStatement of (expression * statementNoShortIf * statement)
	type ifThenElseStatementNoShortIf = IfThenElseStatementNoShortIf of (expression * statementNoShortIf * statementNoShortIf)

	type statement =
		 StatementWithoutTrailingSubstatement of statementWithoutTrailingSubstatement
		 | LabeledStatement of labeledStatement
		 | ifThenStatement
		| `IfThenElseStatement of ifThenElseStatement
		 | WhileStatement of whileStatement
		 | ForStatement of forStatement

	type localVariableDeclarationStatement = LocalVariableDeclarationStatement of localVariableDeclaration
	type localVariableDeclaration = LocalVariableDeclaration of (variableModifiers * jtype * variableDeclarators)

	type blockStatement =
		  localVariableDeclarationStatement
		| ClassOrInterfaceDeclaration of classOrInterfaceDeclaration
		| Statement of statement;;
	type blockStatements = BlockStatements of blockStatement list
	type block = Block of blockStatements
	*)

	type variableDeclaration = Integer of int
	type expression = Bool of bool

	type statement =
			VariableDeclaration of variableDeclaration
		| IfStatement of (expression * (block) * (block))
		| StatementBlock of block
	and block = Block of (statement list)

%}

%token IF ELSE OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACKET CLOSE_BRACKET SEMICOLON EOF

%token <int> INTEGER
%token <bool> BOOLEAN

%start formule
%type <block> formule
%%
formule:
| c=statement EOF { Block([c]) }
ifThenStatement:
| IF OPEN_PARENTHESIS expr=expression CLOSE_PARENTHESIS s=statement { IfStatement(expr, Block([s]), Block([])) }
ifThenElseStatement:
| IF OPEN_PARENTHESIS expr=expression CLOSE_PARENTHESIS s1=statementNoShortIf ELSE s2=statement { IfStatement(expr, Block([s1]), Block([s2])) }
statement:
| s=statementWithoutTrailingSubstatement { s }
| s=ifThenStatement { s }
| s=ifThenElseStatement { s }
statementWithoutTrailingSubstatement:
| b=block { StatementBlock(b) }
statementNoShortIf:
| s=statementWithoutTrailingSubstatement { s }
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
| i=INTEGER { VariableDeclaration(Integer(i)) }
%%
