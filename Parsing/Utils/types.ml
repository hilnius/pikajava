type visibility =
	| Public
  | Protected
  | Private
  | Private_Package

type abstraction =
	| Abstract
	| Concrete

type finality =
	| Final
	| Extendable

type staticity =
	| Static
	| NonStatic

type volatility =
	| Volatile
	| NonVolatile

type synchronization =
	| Synchronized
	| NonSynchronized

type nativity =
	| Native
	| NonNative

type strictfp=
	| StrictFp
	| NonStrictFp

type objType =
	| Class
	| Interface
	| Enum

type child = string option

type identifier = Identifier of string

type annotation = string

type annotationsList = annotation list option


type parameter = {name:identifier; param: parameter option ;extends:parameter option; super:parameter option}

type parameterList = parameter list option

type parentMap = {name:string;parameters:parameterList}

type parent=Parent of parentMap

type modifier =
| Visibility of visibility
| Finality of finality
| Abstraction of abstraction
| Staticity of staticity
| StrictFpity of strictfp
| Synchronization of synchronization
| Nativity of nativity

type modifiers = modifier list option

type argument = {argType: identifier; argName: identifier}

type arguments = argument list option

type exceptionList = identifier list option

(*TODO type classAttribute to be implemented*)
type classAttribute = Empty

type classListAttribute =
	| ClassAttribute of classAttribute
	| Empty

(*TODO type classMethod to be implemented*)

type content= block option

and methodTreeMap = {parameters:parameterList; annots:annotationsList; modif:modifiers; returnType:identifier; name:identifier; args:arguments; thr:exceptionList; con:content }


and initializerTreeMap = {iniType:staticity;con:block}

(* types for blocks *)

and variableDeclaration = Integer of int
and expression = Bool of bool
and classDeclaration = Class

(* end of blocks types *)

and classContentTree =
| MethodTree of methodTreeMap
| Initializer of initializerTreeMap
| ObjectTree of objectTree
| ErrorDecl of string

and objectTree =
ClassTree of classTreeMap
| InterfaceTree of interfaceTreeMap
| EnumTree of enumTreeMap
| ErrorDecl of string

and interfaceTreeMap = {objectType:objType; annots:annotationsList; modif:modifiers; inh:parent list option; interfaceName:identifier; parameters:parameterList; con:contentClass}
and classTreeMap = {objectType:objType; annots:annotationsList; modif:modifiers; parameters:parameterList; inh:parent option; impl:parent list option; className:identifier; con:contentClass}
and enumTreeMap = {objectType:objType; annots:annotationsList; modif:modifiers; inh:parent list option; enumName:identifier; con:contentClass}
and contentClass  = classContentTree list option


and block = Block of blockStatement list
and blockStatement =
    ClassDeclaration of classDeclaration
  | LocalVariableDeclaration of variableDeclaration
  | ClassDeclarationStatement of objectTree
  | Statement of statement
and statement =
    IfStatement of (expression * block * block)
  | ForStatement of (statement * expression * expression * block)
  | WhileStatement of (expression * block)
  | BlockStatement of block
  | EmptyStatement











type classContent = {classContent:classContentTree list option ; attributes:classListAttribute}


