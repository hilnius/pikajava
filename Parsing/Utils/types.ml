
exception SyntaxError of string

type visibility =
  | Public
  | Protected
  | Private
  | Private_Package


and abstraction =
  | Abstract
  | Concrete

and finality =
  | Final
  | Extendable

and staticity =
  | Static
  | NonStatic

and volatility =
  | Volatile
  | NonVolatile

and synchronization =
  | Synchronized
  | NonSynchronized

and nativity =
  | Native
  | NonNative

and strictfp=
  | StrictFp
  | NonStrictFp

and transientity=
  | Transient
  | NonTransient

and objType =
  | Class
  | Interface
  | Enum

and child = string option

and compilationUnit = (packageDeclaration option * importDeclaration list option * classContentTree list option)
and packageDeclaration = (annotations option * packageName)
and importDeclaration =
 | SingleImportDeclaration of typeName
 | TypeImportOnDemandDeclaration of packageOrTypeName
 | SingleStaticImportDeclaration of typeName * identifier
 | StaticImportOnDemandDeclaration of typeName

and annotations = annotation list
and annotation =
  | NormalAnnotation of normalAnnotation
  | MarkerAnnotation of markerAnnotation
  | SingleElementAnnotation of singleElementAnnotation
and normalAnnotation = typeName * elementValuePairs option
and markerAnnotation = typeName
and singleElementAnnotation = typeName * elementValue
and elementValuePairs = elementValuePair list
and elementValuePair = identifier * elementValue
and elementValue =
  | ConditionalExpression of conditionalExpression
  | Annotation of annotation
  | ElementValueArrayInitializer of elementValueArrayInitializer
and elementValueArrayInitializer = elementValue list option
and annotationsList = annotation list option

and annotationTypeDeclaration = identifier * classContentTree list option

and formalParameter = { modifiers: variableModifiers; typed: typed option; declarator: variableDeclarator }

and typeParameterList = typeParameter list
and typeParameter = typeVariable * typeBound option
and typeBound = classOrInterfaceType * classOrInterfaceType list option

and parentMap = { name: identifier; parameters: typeParameterList}

and parent=Parent of parentMap

and modifier =
| Visibility of visibility
| Finality of finality
| Abstraction of abstraction
| Staticity of staticity
| StrictFpity of strictfp
| Synchronization of synchronization
| Nativity of nativity
| Transient of transientity
| Volatile of volatility
| Annotation of annotation

and variableModifiers = modifier list
and methodModifiers = modifier list
and constantModifiers = modifier list
and constructorModifiers = modifier list
and exceptionType = ExceptionClassOrInterfaceType of classOrInterfaceType | ExceptionTypeVariable of typeVariable
and exceptionTypeList = exceptionType list
and throws = exceptionTypeList
(*TODO and classAttribute to be implemented*)
and classAttribute = Empty

and classListAttribute =
  | ClassAttribute of classAttribute
  | Empty

(*TODO types classMethod to be implemented*)

and methodDeclarator = { identifier: identifier; parameters: formalParameter list option }
and methodDeclaration =
| Method of methodImpl
| Constructor of constructorImpl
and methodImpl = { parameters: typeParameterList option; returnType: typed; methodDeclarator: methodDeclarator; thr: exceptionTypeList option; con: block option }
and constructorImpl = { parameters: typeParameterList option; methodDeclarator: methodDeclarator; thr: exceptionTypeList option; con: block option }

and initializerTreeMap = { iniType: staticity; con: block}

(* types for blocks *)

and localVariableDeclaration = (variableModifiers * typed * variableDeclarators)
and variableDeclarators = variableDeclarator list
and variableDeclarator = (identifier * int * variableInitializer option)
and super = Extends of classOrInterfaceType
and interface = classOrInterfaceType
and defaultValue = elementValue
(* end of blocks types *)

and classContentTree =
| InstanceInitializer of block
| StaticInitializer of block
| ModifiedDeclaration of variableModifiers option * declaration
| EmptyContent
and declaration =
| ConstructorDeclaration of (constructorDeclarator * throws option * constructorBody)
| MethodDeclaration of (methodDeclaration * block option)
| ClassDeclaration of classTreeMap
| InterfaceDeclaration of interfaceTreeMap
| FieldDeclaration of typed * variableDeclarators
| EnumDeclaration of enumTreeMap
| AnnotationType of annotationTypeDeclaration
| AnnotationTypeElement of typed * identifier * defaultValue option
| AnnotationTypeDeclaration of classContentTree
| EmptyDeclaration
and constructorDeclarator = (typeParameterList option * typeName * formalParameter list option)
and constructorBody = explicitConstructorInvocation option  * blockStatement list option
and explicitConstructorInvocation = (nonWildTypeArguments option * constructorInvocationType * arguments option)
and constructorInvocationType = This | Super

and interfaceTreeMap = {objectType: objType; inh:interface list option; interfaceName: identifier; parameters: typeParameterList option; con: contentClass}
and classTreeMap = {objectType: objType; parameters: typeParameterList option; super: super option; interfaces: interface list option; className: identifier; con: contentClass}
and enumTreeMap = {objectType: objType; inh:interface list option; enumName: identifier; con: enumContent}
and contentClass  = classContentTree list option
and enumContent = { enumConstants: enumConstant list option; con: contentClass option }
and enumConstant = { annotations : annotation list option; identifier: identifier; arguments: arguments option; classBody: contentClass option }


and block = Block of blockStatement list
and blockStatement =
  | LocalVariableDeclarationStatement of localVariableDeclaration
  | ConstructorInitialization of explicitConstructorInvocation
  | ClassDeclarationStatement of classContentTree
  | Statement of statement
and statement =
    IfStatement of (expression * block * block)
  | ForStatement of (forInit option * expression option * statementExpression list option * block)
  | EnhancedForStatement of (variableModifiers option * typed * identifier * expression * statement)
  | WhileStatement of (expression * block)
  | BlockStatement of block
  | AssertStatement of (expression * expression option)
  | SwitchStatement of (expression * switchCase list)
  | DoWhileStatement of (expression * statement)
  | BreakStatement of identifier option
  | ContinueStatement of identifier option
  | ReturnStatement of expression option
  | ThrowStatement of expression
  | SynchronizedStatement of (expression * block)
  | TryStatement of (block * catch list * block)
  | LabeledStatement of (identifier * statement)
  | ExpressionStatement of (statementExpression)
  | EmptyStatement
and forInit =
  | ForInitStatementExpressionList of statementExpression list
  | ForInitLocalVariableDeclarationStatement of blockStatement
and statementExpression =
  | AssignmentStatement of assignment
  | PreIncrementExpressionStatement of preIncrementExpression
  | PreDecrementExpressionStatement of preDecrementExpression
  | PostIncrementExpressionStatement of postIncrementExpression
  | PostDecrementExpressionStatement of postDecrementExpression
  | MethodInvocationStatement of methodInvocation
  | ClassInstanceCreationExpressionStatement of classInstanceCreationExpression
and switchCase =
  | Case of (expression * blockStatement list)
  | Default of (blockStatement list)
and catch =
  | CatchClause of (formalParameter * block)


and assignmentOperator =
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
  | EqualShiftRightUnsigned

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
  | InfixShiftRightUnsigned
  | InfixMore
  | InfixLess
  | InfixASTERISK
  | InfixDivide
  | InfixModulo

and typed =
    NoneType
  | TypeReference of referenceType
  | TypePrimitive of primitiveType
  | VariadicType of typed

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
  | Void

and identifierArgs =
    IdentifierArgs of (identifier * (typeArgument list))

and identifierTypeArgs =
    IdentifierTypeArgs of (identifier * (typed list))

and literal =
    IntegerLiteral of int
  | FloatingPointLiteral of float
  | BooleanLiteral of bool

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
    ClassInstanceCreationExpression of (typeArguments option * classOrInterfaceType * arguments option * contentClass option)

and methodInvocation =
    MethodInvocationName of (methodName * arguments)
  | MethodInvocationPrimary of (primary * nonWildTypeArguments option * identifier * arguments)
  | MethodInvocationSuper of (nonWildTypeArguments option * identifier * arguments)
  | MethodInvocationClassSuper of (className * nonWildTypeArguments option * identifier * arguments)
  | MethodInvocationType of (typeName * nonWildTypeArguments * identifier * arguments)

and identifier =
    NoneIdentifier
  | Identifier of string

and packageName =
    PackageName of (identifier list)

and packageOrTypeName =
    PackageOrTypeName of (identifier list)

and typeName =
    TypeName of (identifier list)

and expressionName =
    ExpressionName of (identifier list)

and methodName =
    MethodName of (identifier list)

and className =
    ClassName of (identifier list)

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
  | RelationalExpressionLess of (relationalExpression * shiftExpression)
  | RelationalExpressionGreater of (relationalExpression * shiftExpression)
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
  | LeftHandSideArrayAccess of arrayAccess

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

and nonWildTypeArguments =
    NonWildTypeArguments of (referenceType list)

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




type classContent = {classContent:classContentTree list option ; attributes:classListAttribute}


