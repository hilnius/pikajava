open BlocksTypes

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

type parent = string option

type child = string option

type identifier = Identifier of string

type interfacesList = identifier list option

type parameter = {name:identifier; extends:parameter option; super:parameter option}

type parameterList = parameter list option

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

type content= block

type methodTreeMap = {parameters:parameterList; modif:modifiers; returnType:identifier; name:identifier; args:arguments; thr:exceptionList; con:content } 


type initializerTreeMap = {iniType:staticity;con:content}

type classContentTree = 
| MethodTree of methodTreeMap
| Initializer of initializerTreeMap
| ObjectTree of objectTree
| Empty
and objectTree =
ClassTree of classTreeMap
| InterfaceTree of interfaceTreeMap
| EnumTree of enumTreeMap
| Empty
and interfaceTreeMap = {objectType:objType;modif:modifiers; inh:interfacesList; interfaceName:identifier; parameters:parameterList; con:contentClass} 
and classTreeMap = {objectType:objType;modif:modifiers; parameters:parameterList; inh:parent; impl:interfacesList; className:identifier; con:contentClass}
and enumTreeMap = {objectType:objType;modif:modifiers; inh:interfacesList; enumName:identifier; con:contentClass}
and contentClass  = classContentTree list option













type classContent = {classContent:classContentTree list option ; attributes:classListAttribute}


