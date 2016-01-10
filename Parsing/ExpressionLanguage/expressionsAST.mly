%{
  type assignmentOperator =
      NoneAssignmentOperator
    | Equal
    | EqualMore
    | EqualMinus
    | EqualMultiply
    | EqualDivide
    | EqualAnd
    | EqualOr
    | EqualXor
    | EqualModulo
    | EqualLeft
    | EqualRight
    | EqualGGG

  and prefixOp =
      NonePrefix
    | PrefixMoreMore
    | PrefixLessLess
    | PrefixMore
    | PrefixLess
    | PrefixNot
    | PrefixTild

  and postfixOp =
      NonePostfix
    | PostfixMoreMore
    | PostfixLessLess

  and infixOp =
      NoneInfix
    | InfixOr
    | InfixAnd
    | InfixBitOr
    | InfixBitXor
    | InfixBitAnd
    | InfixEqual
    | InfixDifferent
    | InfixLessThan
    | InfixGreaterThan
    | InfixLessEqual
    | InfixGreaterEqual
    | InfixShiftLeft
    | InfixShiftRight
    | InfixGGG
    | InfixMore
    | InfixLess
    | InfixMultiply
    | InfixDivide
    | InfixModulo

  and typed =
      NoneType
    | TypeReference of referenceType
    | TypePrimitive of primitiveType

  and referenceType =
      ReferenceTypeClassOrInterface of classOrInterfaceType
    | ReferenceTypeVariable of typeVariable
    | ReferenceTypeArray of arrayType

  and classOrInterfaceType =
      ClassType of (typeDeclSpecifier * typeArguments)
    | InterfaceType of (typeDeclSpecifier * typeArguments)

  and typeDeclSpecifier =
      TypeDeclSpecifierName of typeName
    | TypeDeclSpecifierIdentifier of (classOrInterfaceType * identifier)

  and typeVariable =
      TypeVariable of identifier

  and arrayType =
      ArrayType of typed

  and typeArguments =
      NoneTypeArguments
    | TypeArguments of (actualTypeArgument list)

  and actualTypeArgument =
      ActualTypeArgumentReferenceType of referenceType
    | ActualTypeArgumentWildcard of wildcard

  and wildcard =
      Wildcard
    | WildcardExtends of referenceType
    | WildcardSuper of referenceType

  and typeArgumentKind =
      NoneTypeArgument
    | TypeArgumentExtends
    | TypeArgumentSuper

  and typeArgument =
      TypeArgumentType of (typed)
    | TypeArgumentGeneric of (typed * typeArgumentKind)

  and primitiveType =
      Byte
    | Short
    | Char
    | Integer
    | Long
    | Float
    | Double
    | Boolean

  and identifierArgs =
      IdentifierArgs of (identifier * (typeArgument list))

  and identifierTypeArgs =
      IdentifierTypeArgs of (identifier * (typed list))

  and variableDeclarators =
      VariableId of string

  and literal =
      IntegerLiteral of int
    | FloatingPointLiteral of float

  and primary =
      PrimaryNoNewArray of primaryNoNewArray
    | PrimaryArrayCreation of arrayCreationExpression

  and primaryNoNewArray =
      PrimaryLiteral of literal
    | PrimaryType of typed
    | PrimaryVoidClass
    | PrimaryThis
    | PrimaryClassThis of className
    | PrimaryExpression of expression
    | PrimaryClassInstanceCreation of classInstanceCreationExpression
    | PrimaryFieldAccess of fieldAccess
    | PrimaryMethodInvocation of methodInvocation
    | PrimaryArrayAccess of arrayAccess

  and arrayCreationExpression =
      ArrayCreationExpression

  and classInstanceCreationExpression =
      ClassInstanceCreationExpression

  and methodInvocation =
      MethodInvocation

  and expression3Cast =
      Expression3CastType of typed
    | Expression3CastExpr of expression

  and expression3infix =
      Expression3Infix of (infixOp * expression3)

  and expression3 =
      Expression3 of (primary * (expression3Cast list) * (prefixOp list) * (postfixOp list) * (selector list))

  and expression2 =
      Expression2 of (expression3)
    | Expression2Infix of (expression3 * (expression3infix list))
    | Expression2InstanceOf of (expression3 * typed)

  and expression1 =
      NoneExpression1
    | Expression1 of (expression2 * expression * expression1)

  and identifier =
      NoneIdentifier
    | Identifier of string

  and packageName =
      PackageName of (identifier list)

  and packageOrTypeName =
      PackageOrTypeName of (identifier list)

  and typeName =
      TypeName of identifier
    | TypeNamePackage of (identifier * packageOrTypeName)

  and expressionName =
      ExpressionName of identifier
    | ExpressionNameAmbiguous of (identifier * ambiguousName)

  and methodName =
      MethodName of identifier
    | MethodNameAmbiguous of (identifier * ambiguousName)

  and className =
      ClassName of identifier
    | ClassNameAmbiguous of (identifier * ambiguousName)

  and ambiguousName =
      AmbiguousName of (identifier list)

  and expression = 
      NoneExpression
    | ExpressionAssignment of assignmentExpression

  and assignmentExpression =
      AssignmentExpressionAssignment of assignment
    | AssignmentExpressionConditional of conditionalExpression

  and conditionalExpression =
      ConditionalExpression of conditionalOrExpression
    | ConditionalExpressionTernary of (conditionalOrExpression * expression * conditionalExpression)

  and conditionalOrExpression =
      ConditionalOrExpression of (conditionalAndExpression list)

  and conditionalAndExpression =
      ConditionalAndExpression of (inclusiveOrExpression list)

  and inclusiveOrExpression =
      InclusiveOrExpression of (exclusiveOrExpression list)

  and exclusiveOrExpression =
      ExclusiveOrExpression of (andExpression list)

  and andExpression =
      AndExpression of (equalityExpression list)

  and equalityExpression =
      EqualityExpression of relationalExpression
    | EqualityExpressionEqual of (equalityExpression * relationalExpression)
    | EqualityExpressionDifferent of (equalityExpression * relationalExpression)

  and relationalExpression =
      RelationalExpression of shiftExpression
    | RelationalExpressionLessThan of (relationalExpression * shiftExpression)
    | RelationalExpressionGreaterThan of (relationalExpression * shiftExpression)
    | RelationalExpressionLessEqualThan of (relationalExpression * shiftExpression)
    | RelationalExpressionGreaterEqualThan of (relationalExpression * shiftExpression)
    | RelationalExpressionInstanceOf of (relationalExpression * referenceType)

  and shiftExpression =
      ShiftExpression of additiveExpression
    | ShiftExpressionLeft of (shiftExpression * additiveExpression)
    | ShiftExpressionRight of (shiftExpression * additiveExpression)
    | ShiftExpressionUnsignedRight of (shiftExpression * additiveExpression)

  and additiveExpression =
      AdditiveExpression of multiplicativeExpression
    | AdditiveExpressionAdd of (additiveExpression * multiplicativeExpression)
    | AdditiveExpressionSubstract of (additiveExpression * multiplicativeExpression)

  and multiplicativeExpression =
      MultiplicativeExpression of unaryExpression
    | MultiplicativeExpressionMultiply of (multiplicativeExpression * unaryExpression)
    | MultiplicativeExpressionDivide of (multiplicativeExpression * unaryExpression)
    | MultiplicativeExpressionModulo of (multiplicativeExpression * unaryExpression)

  and unaryExpression =
      UnaryExpressionPreInc of preIncrementExpression
    | UnaryExpressionPreDec of preDecrementExpression
    | UnaryExpressionPlus of unaryExpression
    | UnaryExpressionMinus of unaryExpression
    | UnaryExpressionNotPlusMinus of unaryExpressionNotPlusMinus

  and preIncrementExpression =
      PreIncrementExpression of unaryExpression

  and preDecrementExpression =
      PreDecrementExpression of unaryExpression

  and unaryExpressionNotPlusMinus =
      UnaryExpressionNotPlusMinusPostfix of postfixExpression
    | UnaryExpressionNotPlusMinusBitnot of unaryExpression
    | UnaryExpressionNotPlusMinusNot of unaryExpression
    | UnaryExpressionNotPlusMinusCast of castExpression

  and castExpression =
      CastExpressionPrimitive of (primitiveType * int * unaryExpression)
    | CastExpressionReference of (referenceType * unaryExpressionNotPlusMinus)

  and postfixExpression =
      PostfixExpressionPrimary of primary
    | PostfixExpressionName of expressionName
    | PostfixExpressionPostInc of postIncrementExpression
    | PostfixExpressionPostDec of postDecrementExpression

  and postIncrementExpression =
      PostIncrementExpression of postfixExpression

  and postDecrementExpression =
      PostDecrementExpression of postfixExpression

  and assignment =
    | Assignment of (leftHandSide * assignmentOperator * assignmentExpression)

  and leftHandSide =
      LeftHandSideExpressionName of expressionName
    | LeftHandSideFieldAccess of fieldAccess
    | LeftHandSideArrayAccess

  and fieldAccess =
      FieldAccessPrimary of (primary * identifier)
    | FieldAccessSuper of (identifier)
    | FieldAccessClass of (className * identifier)

  and arrayAccess =
      ArrayAccessExpression of (expressionName * expression)
    | ArrayAccessPrimary of (primaryNoNewArray * expression)

  and selector =
      NoneSelector
    | SelectorIdentifier of (identifier * arguments)
    | SelectorInvocation of explicitGenericInvocation
    | SelectorThis
    | SelectorSuper of superSuffix
    | SelectorNew of (innerCreator * (typed list) * expression)

  and arguments =
      NoneArguments
    | Arguments of (expression list)

  and superSuffix =
      NoneSuperSuffix
    | SuperSuffixArguments of arguments
    | SuperSuffixIdentifier of (identifier * arguments)

  and identifierSuffix =
      NoneIdentifierSuffix
    | IdentifierSuffixArrayClass of int
    | IdentifierSuffixArrayExpression of expression
    | IdentifierSuffixArguments of arguments
    | IdentifierSuffixDotClass
    | IdentifierSuffixDotInvocation of explicitGenericInvocation
    | IdentifierSuffixDotThis
    | IdentifierSuffixDotSuperArguments of arguments
    | IdentifierSuffixDotNew of (innerCreator * (typed list))

  and explicitGenericInvocation =
      ExplicitGenericInvocation of ((typed list) * explicitGenericInvocationSuffix)

  and explicitGenericInvocationSuffix =
      ExplicitGenericInvocationSuffixSuper of superSuffix
    | ExplicitGenericInvocationSuffixIdentifier of (identifier * arguments)

  and creator =
      NoneCreator
    | CreatorArray of (createdName * (typed list) * arrayCreatorRest)
    | CreatorClass of (createdName * (typed list) * classCreatorRest)

  and createdName =
      CreatedName of (identifier * (typed list) * (identifierTypeArgs list))

  and innerCreator =
      InnerCreator of (identifier * classCreatorRest)

  and arrayCreatorRest =
      ArrayCreatorRestInit of (arrayInitializer * int)
    | ArrayCreatorRestExpression of ((expression list) * int)

  and classCreatorRest =
      ClassCreatorRest of (arguments * classBody)

  and arrayInitializer =
      ArrayInitializer of (variableInitializer list)
    | ArrayInitializerTrailing of (variableInitializer list)

  and variableInitializer =
      VariableInitializerArray of arrayInitializer
    | VariableInitializerExpression of expression

  and classBody =
      NoneClassBody
    | ClassBody
%}

%token EOF

%token EQUAL MORE_EQUAL LESS_EQUAL MULTIPLY_EQUAL DIVIDE_EQUAL AND_EQUAL OR_EQUAL XOR_EQUAL MODULO_EQUAL LEFT_EQUAL RIGHT_EQUAL GGG_EQUAL
%token OR AND BITOR BITXOR BITAND EQUALEQUAL DIFFERENT LESSEQUALTHAN GREATEREQUALTHAN SHIFTLEFT SHIFTRIGHT GGG MULTIPLY DIVIDE MODULO
%token EXCLAMATION_MARK QUESTION_MARK TWO_DOTS DOT COMMA GREATERTHAN LESSTHAN
%token MOREMORE LESSLESS MORE LESS TILD


%token PAROPEN PARCLOSE BRACKETOPEN BRACKETCLOSE BRACEOPEN BRACECLOSE

%token VOID CLASS NEW EXTENDS THIS SUPER INSTANCEOF NULL

%token BYTE SHORTINT INTEGER LONGINT CHAR STRING FLOAT DOUBLE BOOLEAN

%token <int> INTEGER_NUMERAL
%token <float> FLOATING_POINT_NUMERAL
%token <bool> BOOLEAN_LITERAL
%token <string> IDENTIFIER

%start perform
%type <expression> perform
%%
perform:
| p=expression EOF { p }

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
| p=conditionalOrExpression QUESTION_MARK e=expression TWO_DOTS c=conditionalExpression
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
| p=relationalExpression LESSTHAN s=shiftExpression
	{ RelationalExpressionLessThan(p, s) }
| p=relationalExpression GREATERTHAN s=shiftExpression
	{ RelationalExpressionGreaterThan(p, s) }
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
| p=shiftExpression GGG a=additiveExpression
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
| p=multiplicativeExpression MULTIPLY u=unaryExpression
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
| PAROPEN t=primitiveType c=dimsopt PARCLOSE p=unaryExpression
	{ CastExpressionPrimitive(t, c, p) }
| PAROPEN t=referenceType PARCLOSE p=unaryExpressionNotPlusMinus
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
| PAROPEN p=expression PARCLOSE
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
| MULTIPLY_EQUAL 
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
| GGG_EQUAL 
	{ EqualGGG }

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
| LESSTHAN 
	{ InfixLessThan }
| GREATERTHAN 
	{ InfixGreaterThan }
| LESSEQUALTHAN
	{ InfixLessEqual }
| GREATEREQUALTHAN
	{ InfixGreaterEqual }
| SHIFTLEFT 
	{ InfixShiftLeft }
| SHIFTRIGHT 
	{ InfixShiftRight }
| GGG 
	{ InfixGGG }
| MORE 
	{ InfixMore }
| LESS 
	{ InfixLess }
| MULTIPLY 
	{ InfixMultiply }
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
| LESSTHAN p=actualTypeArgumentList GREATERTHAN
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
| p=identifier LESSTHAN a=separated_nonempty_list(COMMA, typeArgument) GREATERTHAN 
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

identifier:
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
| DOT NEW e=optNonWildcardTypeArguments p=innerCreator 
	{ IdentifierSuffixDotNew(p, e) }

explicitGenericInvocation:
| p=nonWildcardTypeArguments e=explicitGenericInvocationSuffix 
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
| DOT NEW a=optNonWildcardTypeArguments p=innerCreator 
	{ SelectorNew(p, a, NoneExpression) }
| DOT NEW a=optNonWildcardTypeArguments p=innerCreator e=expression 
	{ SelectorNew(p, a, e) }

arguments:
| PAROPEN p=separated_list(COMMA, expression) PARCLOSE 
	{ Arguments(p) }

optNonWildcardTypeArguments:
| 
	{ [] }
| p=nonWildcardTypeArguments 
	{ p }

nonWildcardTypeArguments:
| LESSTHAN GREATERTHAN 
	{ [] }
| LESSTHAN p=typeList GREATERTHAN 
	{ p }

creator:
| a=optNonWildcardTypeArguments p=createdName r=arrayCreatorRest 
	{ CreatorArray(p, a, r) }
| a=optNonWildcardTypeArguments p=createdName r=classCreatorRest 
	{ CreatorClass(p, a, r) }

createdName:
| p=identifier a=optNonWildcardTypeArguments l=createdNameList 
	{ CreatedName(p, a, l) }

createdNameList:
| 
	{ [] }
| e=createdNameList DOT p=identifier a=optNonWildcardTypeArguments 
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
| BRACEOPEN l=variableInitList BRACECLOSE 
	{ ArrayInitializer l }
| BRACEOPEN l=variableInitList COMMA BRACECLOSE 
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


