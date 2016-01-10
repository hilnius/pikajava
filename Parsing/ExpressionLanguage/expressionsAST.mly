%{
  open Location
  open Located
  open ExitManagement
%}

%start expression
%type <Types.expression> expression
%%
expression:
| p=assignmentExpression
	{ ExpressionAssignment p }

assignmentExpression:
| p=conditionalExpression
	{ AssignmentExpressionConditional p }
| p=assignment
	{ AssignmentExpressionAssignment p }

conditionalExpression:
| p=conditionalOrExpression
	{ ConditionalExpression p }
| p=conditionalOrExpression QUESTION_MARK e=expression COLON c=conditionalExpression
	{ ConditionalExpressionTernary(p, e, c) }

conditionalOrExpression:
| p=conditionalAndExpression
	{ ConditionalOrExpression [p] }
| o=conditionalOrExpression OR p=conditionalAndExpression
	{ let ConditionalOrExpression(e) = o in ConditionalOrExpression( p::e ) }

conditionalAndExpression:
| p=inclusiveOrExpression
	{ ConditionalAndExpression [p] }
| o=conditionalAndExpression AND p=inclusiveOrExpression
	{ let ConditionalAndExpression(e) = o in ConditionalAndExpression( p::e ) }

inclusiveOrExpression:
| p=exclusiveOrExpression
	{ InclusiveOrExpression [p] }
| o=inclusiveOrExpression BITOR p=exclusiveOrExpression
	{ let InclusiveOrExpression(e) = o in InclusiveOrExpression( p::e  ) }

exclusiveOrExpression:
| p=andExpression
	{ ExclusiveOrExpression [p] }
| o=exclusiveOrExpression BITXOR p=andExpression
	{ let ExclusiveOrExpression(e) = o in ExclusiveOrExpression( p::e ) }

andExpression:
| p=equalityExpression
	{ AndExpression [p] }
| o=andExpression BITAND p=equalityExpression
	{ let AndExpression(e) = o in AndExpression( p::e ) }

equalityExpression:
| p=relationalExpression
	{ EqualityExpression p }
| p=equalityExpression EQUALEQUAL r=relationalExpression
	{ EqualityExpressionEqual(p, r) }
| p=equalityExpression DIFFERENT r=relationalExpression
	{ EqualityExpressionDifferent(p, r) }

relationalExpression:
| p=shiftExpression
	{ RelationalExpression p }
| p=relationalExpression OPENING_CHEVRON s=shiftExpression
	{ RelationalExpressionLess(p, s) }
| p=relationalExpression CLOSING_CHEVRON s=shiftExpression
	{ RelationalExpressionGreater(p, s) }
| p=relationalExpression LESSEQUALTHAN s=shiftExpression
	{ RelationalExpressionLessEqualThan(p, s) }
| p=relationalExpression GREATEREQUALTHAN s=shiftExpression
	{ RelationalExpressionGreaterEqualThan(p, s) }
| p=relationalExpression INSTANCEOF t=referenceType
	{ RelationalExpressionInstanceOf(p, t) }

shiftExpression:
| p=additiveExpression
	{ ShiftExpression p }
| p=shiftExpression SHIFTLEFT a=additiveExpression
	{ ShiftExpressionLeft(p, a) }
| p=shiftExpression SHIFTRIGHT a=additiveExpression
	{ ShiftExpressionRight(p, a) }
| p=shiftExpression SHIFT_RIGHT_UNSIGNED a=additiveExpression
	{ ShiftExpressionUnsignedRight(p, a) }

additiveExpression:
| p=multiplicativeExpression
	{ AdditiveExpression p }
| p=additiveExpression MORE m=multiplicativeExpression
	{ AdditiveExpressionAdd(p, m) }
| p=additiveExpression LESS m=multiplicativeExpression
	{ AdditiveExpressionSubstract(p, m) }

multiplicativeExpression:
| p=unaryExpression
	{ MultiplicativeExpression p }
| p=multiplicativeExpression ASTERISK u=unaryExpression
	{ MultiplicativeExpressionMultiply(p, u) }
| p=multiplicativeExpression DIVIDE u=unaryExpression
	{ MultiplicativeExpressionDivide(p, u) }
| p=multiplicativeExpression MODULO u=unaryExpression
	{ MultiplicativeExpressionModulo(p, u) }

unaryExpression:
| p=preIncrementExpression
	{ UnaryExpressionPreInc p }
| p=preDecrementExpression
	{ UnaryExpressionPreDec p }
| MORE p=unaryExpression
	{ UnaryExpressionPlus p }
| LESS p=unaryExpression
	{ UnaryExpressionMinus p }
| p=unaryExpressionNotPlusMinus
	{ UnaryExpressionNotPlusMinus p }

preIncrementExpression:
| MOREMORE p=unaryExpression
	{ PreIncrementExpression p }

preDecrementExpression:
| LESSLESS p=unaryExpression
	{ PreDecrementExpression p }

unaryExpressionNotPlusMinus:
| p=postfixExpression
	{ UnaryExpressionNotPlusMinusPostfix p }
| TILD p=unaryExpression
	{ UnaryExpressionNotPlusMinusBitnot p }
| EXCLAMATION_MARK p=unaryExpression
	{ UnaryExpressionNotPlusMinusNot p }
| p=castExpression
	{ UnaryExpressionNotPlusMinusCast p }

castExpression:
| OPENING_PARENTHESIS t=primitiveType c=dimsopt CLOSING_PARENTHESIS p=unaryExpression
	{ CastExpressionPrimitive(t, c, p) }
| OPENING_PARENTHESIS t=referenceType CLOSING_PARENTHESIS p=unaryExpressionNotPlusMinus
	{ CastExpressionReference(t, p) }

postfixExpression:
| p=primary
	{ PostfixExpressionPrimary p }
| p=expressionName
	{ PostfixExpressionName p }
| p=postIncrementExpression
	{ PostfixExpressionPostInc p }
| p=postDecrementExpression
	{ PostfixExpressionPostDec p }

postIncrementExpression:
| p=postfixExpression MOREMORE
	{ PostIncrementExpression p }

postDecrementExpression:
| p=postfixExpression LESSLESS
	{ PostDecrementExpression p }

/*:
| p=
	{  p }

:
| p=
	{  p }

:
| p=
	{  p }

:
| p=
	{  p }*/

assignment:
| l=leftHandSide o=assignmentOperator p=assignmentExpression
	{ Assignment(l, o, p) }

leftHandSide:
| p=expressionName
	{ LeftHandSideExpressionName p }
| p=fieldAccess
	{ LeftHandSideFieldAccess p }
//| p=arrayAccess
//	{ LeftHandSideArrayAccess p }

fieldAccess:
| p=primary DOT i=identifier
        { FieldAccessPrimary(p, i) }
| SUPER DOT p=identifier
        { FieldAccessSuper p }
| p=className DOT SUPER DOT i=identifier
        { FieldAccessClass(p, i) }

primary:
| p=primaryNoNewArray
	{ PrimaryNoNewArray p }
//| p=arrayCreationExpression
//	{ PrimaryArrayCreation p }

primaryNoNewArray:
| p=literal
	{ PrimaryLiteral p }
| p=typed DOT CLASS
	{ PrimaryType p }
| VOID DOT CLASS
	{ PrimaryVoidClass }
| THIS
	{ PrimaryThis }
| p=className DOT THIS
	{ PrimaryClassThis p }
| OPENING_PARENTHESIS p=expression CLOSING_PARENTHESIS
	{ PrimaryExpression p }
//| p=classInstanceCreationExpression
//	{ PrimaryClassInstanceCreation p }
| p=fieldAccess
	{ PrimaryFieldAccess p }
//| p=methodInvocation
//	{ PrimaryMethodInvocation p }
//| p=arrayAccess
//	{ PrimaryArrayAccess p }

dims:
| BRACKETOPEN BRACKETCLOSE
	{ 1 }
| p=dims BRACKETOPEN BRACKETCLOSE
	{ p + 1 }

dimsopt:
|
	{ 0 }
| p=dims
	{ p }

assignmentOperator:
| EQUAL
	{ Equal }
| MORE_EQUAL
	{ EqualMore }
| LESS_EQUAL
	{ EqualMinus }
| ASTERISK_EQUAL
	{ EqualMultiply }
| DIVIDE_EQUAL
	{ EqualDivide }
| AND_EQUAL
	{ EqualAnd }
| OR_EQUAL
	{ EqualOr }
| XOR_EQUAL
	{ EqualXor }
| MODULO_EQUAL
	{ EqualModulo }
| LEFT_EQUAL
	{ EqualLeft }
| RIGHT_EQUAL
	{ EqualRight }
| SHIFT_RIGHT_UNSIGNED_EQUAL
	{ EqualShiftRightUnsigned }

/*prefixop:
| MOREMORE
	{ PrefixMoreMore }
| LESSLESS
	{ PrefixLessLess }
| MORE
	{ PrefixMore }
| LESS
	{ PrefixLess }
| EXCLAMATION_MARK
	{ PrefixNot }
| TILD
	{ PrefixTild }

postfixop:
| MOREMORE
	{ PostfixMoreMore }
| LESSLESS
	{ PostfixLessLess }

infixop:
| OR
	{ InfixOr }
| AND
	{ InfixAnd }
| BITOR
	{ InfixBitOr }
| BITXOR
	{ InfixBitXor }
| BITAND
	{ InfixBitAnd }
| EQUALEQUAL
	{ InfixEqual }
| DIFFERENT
	{ InfixDifferent }
| OPENING_CHEVRON
	{ InfixLessThan }
| CLOSING_CHEVRON
	{ InfixGreaterThan }
| LESSEQUALTHAN
	{ InfixLessEqual }
| GREATEREQUALTHAN
	{ InfixGreaterEqual }
| SHIFTLEFT
	{ InfixShiftLeft }
| SHIFTRIGHT
	{ InfixShiftRight }
| SHIFT_RIGHT_UNSIGNED
	{ InfixShiftRightUnsigned }
| MORE
	{ InfixMore }
| LESS
	{ InfixLess }
| ASTERISK
	{ InfixASTERISK }
| DIVIDE
	{ InfixDivide }
| MODULO
	{ InfixModulo }*/

/*identifierList:
| p=identifier
	{ [p] }
| e=identifierList DOT p=identifier
	{ p::e }*/

literal:
| p=INTEGER_NUMERAL
	{ IntegerLiteral(p) }
| p=FLOATING_POINT_NUMERAL
	{ FloatingPointLiteral(p) }

typed:
| p=primitiveType
	{ TypePrimitive p }
| p=referenceType
	{ TypeReference p }

primitiveType:
| p=numericType
	{ p }
| BOOLEAN
	{ Boolean }

numericType:
| p=integralType
	{ p }
| p=floatingPointType
	{ p }

integralType:
| BYTE
	{ Byte }
| SHORTINT
	{ Short }
| INTEGER
	{ Integer }
| LONGINT
	{ Long }
| CHAR
	{ Char }

floatingPointType:
| FLOAT
	{ Float }
| DOUBLE
	{ Double }

referenceType:
| p=classOrInterfaceType
	{ ReferenceTypeClassOrInterface p }
| p=typeVariable
	{ ReferenceTypeVariable p }
| p=arrayType
	{ ReferenceTypeArray p }

classOrInterfaceType:
| p=classType
	{ p }
/*| p=interfaceType
	{ p }*/

classType:
| p=typeDeclSpecifier a=typeArgumentsOpt
	{ ClassType(p, a) }

/*interfaceType:
| p=typeDeclSpecifier a=typeArgumentsOpt
	{ InterfaceType(p, a) }*/

typeDeclSpecifier:
| p=typeName
	{ TypeDeclSpecifierName p }
| t=classOrInterfaceType DOT p=identifier
	{ TypeDeclSpecifierIdentifier(t, p) }

typeVariable:
| p=identifier
	{ TypeVariable p }

arrayType:
| p=typed BRACKETOPEN BRACKETCLOSE
	{ ArrayType p }

typeArgumentsOpt:
|
	{ NoneTypeArguments }
| p=typeArguments
	{ p }

typeArguments:
| OPENING_CHEVRON p=actualTypeArgumentList CLOSING_CHEVRON
	{ TypeArguments p }

actualTypeArgumentList:
| p=actualTypeArgument
	{ [p] }
| e=actualTypeArgumentList COMMA p=actualTypeArgument
	{ p::e }

actualTypeArgument:
| p=referenceType
	{ ActualTypeArgumentReferenceType p }
| p=wildcard
	{ ActualTypeArgumentWildcard p }

wildcard:
| QUESTION_MARK
	{ Wildcard }
| QUESTION_MARK EXTENDS p=referenceType
	{ WildcardExtends p }
| QUESTION_MARK SUPER p=referenceType
	{ WildcardSuper p }

/*typedList:
| p=typedIdarg
	{ [p] }
| e=typedList DOT p=typedIdarg
	{ p::e }

typedIdarg:
| p=identifier
	{ IdentifierArgs(p, []) }
| p=identifier OPENING_CHEVRON a=separated_nonempty_list(COMMA, typeArgument) CLOSING_CHEVRON
	{ IdentifierArgs(p, a) }

typeArgument:
| p=typed
	{ TypeArgumentType p }
| QUESTION_MARK
	{ TypeArgumentGeneric(NoneType, NoneTypeArgument) }
| QUESTION_MARK EXTENDS p=typed
	{ TypeArgumentGeneric(p, TypeArgumentExtends) }
| QUESTION_MARK SUPER p=typed
	{ TypeArgumentGeneric(p, TypeArgumentSuper) }

typeList:
| p=typed
	{ [p] }
| e=typeList p=typed
	{ p::e }*/

%public identifier:
| p=IDENTIFIER
	{ Identifier(p) }

/*packageName:
| p=identifier
	{ PackageName [p] }
| e=packageName DOT p=identifier
	{ let PackageName(l) = e in PackageName(p::l) }
*/
packageOrTypeName:
| p=identifier
	{ PackageOrTypeName [p] }
| e=packageOrTypeName DOT p=identifier
	{ let PackageOrTypeName(l) = e in PackageOrTypeName(p::l) }

ambiguousName:
| p=identifier
	{ AmbiguousName [p] }
| e=ambiguousName DOT p=identifier
	{ let AmbiguousName(l) = e in AmbiguousName(p::l) }

expressionName:
| p=identifier
	{ ExpressionName p }
| a=expressionName DOT p=identifier
	{ ExpressionNameAmbiguous(p, AmbiguousName( [] )) }

/*methodName:
| p=identifier
	{ MethodName p }
| a=ambiguousName DOT p=identifier
	{ MethodNameAmbiguous(p, a) }
*/
typeName:
| p=identifier
	{ TypeName p }
| a=packageOrTypeName DOT p=identifier
	{ TypeNamePackage(p, a) }

%inline className:
| p=identifier
	{ ClassName p }
//| a=ambiguousName DOT p=identifier
//	{ ClassNameAmbiguous(p, a) }

/*identifierSuffix:
| BRACKETOPEN BRACKETCLOSE c=arrayCounter DOT CLASS
	{ IdentifierSuffixArrayClass(c + 1) }
| BRACKETOPEN e=expression BRACKETCLOSE
	{ IdentifierSuffixArrayExpression e }
| p=arguments
	{ IdentifierSuffixArguments p }
| DOT CLASS
	{ IdentifierSuffixDotClass }
| DOT p=explicitGenericInvocation
	{ IdentifierSuffixDotInvocation p }
| DOT THIS
	{ IdentifierSuffixDotThis }
| DOT SUPER p=arguments
	{ IdentifierSuffixDotSuperArguments p }
| DOT NEW e=optNonQUESTION_MARKTypeArguments p=innerCreator
	{ IdentifierSuffixDotNew(p, e) }

explicitGenericInvocation:
| p=nonQUESTION_MARKTypeArguments e=explicitGenericInvocationSuffix
	{ ExplicitGenericInvocation(p, e) }

explicitGenericInvocationSuffix:
| SUPER p=superSuffix
	{ ExplicitGenericInvocationSuffixSuper p }
| p=identifier a=arguments
	{ ExplicitGenericInvocationSuffixIdentifier(p, a) }

superSuffix:
| a=arguments
	{ SuperSuffixArguments a }
| DOT p=identifier a=arguments
	{ SuperSuffixIdentifier(p, a) }

selector:
| DOT p=identifier
	{ SelectorIdentifier(p, NoneArguments) }
| DOT p=identifier a=arguments
	{ SelectorIdentifier(p, a) }
| DOT p=explicitGenericInvocation
	{ SelectorInvocation p }
| DOT THIS
	{ SelectorThis }
| DOT SUPER p=superSuffix
	{ SelectorSuper p }
| DOT NEW a=optNonQUESTION_MARKTypeArguments p=innerCreator
	{ SelectorNew(p, a, NoneExpression) }
| DOT NEW a=optNonQUESTION_MARKTypeArguments p=innerCreator e=expression
	{ SelectorNew(p, a, e) }
*/
%public arguments:
| OPENING_PARENTHESIS p=separated_list(COMMA, expression) CLOSING_PARENTHESIS
	{ Arguments(p) }
/*
optNonQUESTION_MARKTypeArguments:
|
	{ [] }
| p=nonQUESTION_MARKTypeArguments
	{ p }

nonQUESTION_MARKTypeArguments:
| OPENING_CHEVRON CLOSING_CHEVRON
	{ [] }
| OPENING_CHEVRON p=typeList CLOSING_CHEVRON
	{ p }

creator:
| a=optNonQUESTION_MARKTypeArguments p=createdName r=arrayCreatorRest
	{ CreatorArray(p, a, r) }
| a=optNonQUESTION_MARKTypeArguments p=createdName r=classCreatorRest
	{ CreatorClass(p, a, r) }

createdName:
| p=identifier a=optNonQUESTION_MARKTypeArguments l=createdNameList
	{ CreatedName(p, a, l) }

createdNameList:
|
	{ [] }
| e=createdNameList DOT p=identifier a=optNonQUESTION_MARKTypeArguments
	{ IdentifierTypeArgs(p, a)::e }

innerCreator:
| p=identifier r=classCreatorRest
	{ InnerCreator(p, r) }

classCreatorRest:
| a=arguments
	{ ClassCreatorRest(a, NoneClassBody) }
// TC classBody

arrayCreatorRest:
| BRACKETOPEN BRACKETCLOSE c=arrayCounter p=arrayInitializer
	{ ArrayCreatorRestInit(p, c) }
| BRACKETOPEN p=expression BRACKETCLOSE e=arrayCreatorRestList c=arrayCounter
	{ ArrayCreatorRestExpression(p::e, c) }

arrayCreatorRestList:
|
	{ [] }
| e=arrayCreatorRestList BRACKETOPEN p=expression BRACKETCLOSE
	{ p::e }

arrayCounter:
|
	{ 0 }
| p=arrayCounter BRACKETOPEN BRACKETCLOSE
	{ p + 1 }

arrayInitializer:
| OPENING_BRACE l=variableInitList CLOSING_BRACE
	{ ArrayInitializer l }
| OPENING_BRACE l=variableInitList COMMA CLOSING_BRACE
	{ ArrayInitializerTrailing l }

variableInitList:
|
	{ [] }
| e=variableInitList COMMA p=variableInitializer
	{ p::e }

variableInitializer:
| p=arrayInitializer
	{ VariableInitializerArray p }
| p=expression
	{ VariableInitializerExpression p }*/

%%


