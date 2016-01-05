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

(*TODO type classAttribute to be implemented*)
type classAttribute = Empty

type classListAttribute =
	| ClassAttribute of classAttribute
	| Empty


(*TODO type classMethod to be implemented*)
type classMethod = Method of string
type classListMethod =
	| ClassMethod of classMethod
	| Empty

type content  = (classListAttribute * classListMethod ) option

type interfaceTreeMap = {objectType:objType;vis:visibility; inh:interfacesList; interfaceName:identifier; parameters:parameterList; con:content}

type classTreeMap = {objectType:objType;vis:visibility; abs:abstraction; fin:finality; parameters:parameterList; inh:parent; impl:interfacesList; className:identifier; con:content}

type enumTreeMap = {objectType:objType;vis:visibility; inh:interfacesList; enumName:identifier; con:content}

type objectTree =
ClassTree of classTreeMap
| InterfaceTree of interfaceTreeMap
| EnumTree of enumTreeMap
| Empty


type classContent = {methods:classListMethod; attributes:classListAttribute}


