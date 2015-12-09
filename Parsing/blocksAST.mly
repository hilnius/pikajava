%{

	(* type statementExpression =
		  Assignment of assignment *)
		(* | PreIncrementExpression of preIncrementExpression *)
		(* | PreDecrementExpression of preDecrementExpression *)
		(* | PostIncrementExpression of postIncrementExpression *)
		(* | PostDecrementExpression of postDecrementExpression *)
		(* | MethodInvocation of methodInvocation *)
		(* | ClassInstanceCreationExpression of classInstanceCreationExpression *)

	(* type expressionStatement = StatementExpression of statementExpression *)

	type statementWithoutTrailingSubstatement =
		  Block of block
		(* | EmptyStatement of emptyStatement *)
		(* | ExpressionStatement of expressionStatement *)
		(* | AssertStatement of assertStatement *)
		(* | SwitchStatement of switchStatement *)
		(* | DoStatement of doStatement *)
		(* | BreakStatement of breakStatement *)
		(* | ContinueStatement of continueStatement *)
		(* | ReturnStatement of returnStatement *)
		(* | SynchronizedStatement of synchronizedStatement *)
		(* | ThrowStatement of throwStatement *)
		(* | TryStatement of tryStatement *)

	type statementNoShortIf =
		  StatementWithoutTrailingSubstatement of statementWithoutTrailingSubstatement
		(* | LabeledStatementNoShortIf of labeledStatementNoShortIf *)
		| IfThenElseStatementNoShortIf of ifThenElseStatementNoShortIf
		(* | WhileStatementNoShortIf of whileStatementNoShortIf *)
		(* | ForStatementNoShortIf of forStatementNoShortIf *)

	(* type ifThenStatement = IfThenStatement of (expression * statement) *)
	type ifThenElseStatement = `IfThenElseStatement of (expression * statementNoShortIf * statement)
	type ifThenElseStatementNoShortIf = IfThenElseStatementNoShortIf of (expression * statementNoShortIf * statementNoShortIf)

	type statement =
		 StatementWithoutTrailingSubstatement of statementWithoutTrailingSubstatement
		(* | LabeledStatement of labeledStatement *)
		(* | ifThenStatement *)
		| `IfThenElseStatement of ifThenElseStatement
		(* | WhileStatement of whileStatement *)
		(* | ForStatement of forStatement *)

	type localVariableDeclarationStatement = LocalVariableDeclarationStatement of localVariableDeclaration
	type localVariableDeclaration = LocalVariableDeclaration of (variableModifiers * jtype * variableDeclarators)

	type blockStatement =
		  localVariableDeclarationStatement
		(*| ClassOrInterfaceDeclaration of classOrInterfaceDeclaration*)
		| Statement of statement;;
	type blockStatements = BlockStatements of blockStatement list
	type block = Block of blockStatements

%}

%start formule
%type <operation> formule
%%
formule:
| c=primaryOperation EOF { c }
primaryOperation:
| p=primaryOperation ADD s=secondaryOperation { Computation(p, ADD, s) }
| p=primaryOperation SUB s=secondaryOperation { Computation(p, SUB, s) }
| s=secondaryOperation { s }
secondaryOperation:
| s=secondaryOperation MUL vi=var_or_int { Computation(s, MUL, Value(vi)) }
| s=secondaryOperation DIV vi=var_or_int { Computation(s, DIV, Value(vi)) }
| vi=var_or_int { Value(vi) }
var_or_int:
| v=VAR { Variable(v) }
| i=INT { Number(i) }
%%
