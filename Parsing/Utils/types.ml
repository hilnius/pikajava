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

type methodTreeMap = {parameters:parameterList; annots:annotationsList; modif:modifiers; returnType:identifier; name:identifier; args:arguments; thr:exceptionList; con:content } 


type initializerTreeMap = {iniType:staticity;con:block}

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
and interfaceTreeMap = {objectType:objType; annots:annotationsList; modif:modifiers; inh:parent list option; interfaceName:identifier; parameters:parameterList; con:contentClass} 
and classTreeMap = {objectType:objType; annots:annotationsList; modif:modifiers; parameters:parameterList; inh:parent option; impl:parent list option; className:identifier; con:contentClass}
and enumTreeMap = {objectType:objType; annots:annotationsList; modif:modifiers; inh:parent list option; enumName:identifier; con:contentClass}
and contentClass  = classContentTree list option













type classContent = {classContent:classContentTree list option ; attributes:classListAttribute}


