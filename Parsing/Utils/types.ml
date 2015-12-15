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

type parent = string option

type identifier = Identifier of string

type interfacesList = identifier list option

(*TODO type classAttribute to be implemented*)
type classAttribute = Empty
type classListAttribute =
	| ClassAttribute of classAttribute
	| Empty


(*TODO type classMethod to be implemented*)
type classMethod = Empty
type classListMethod =
	| ClassMethod of classMethod
	| Empty

type content  = (classListAttribute * classListMethod ) option

type classTreeMap = {vis:visibility; abs:abstraction; fin:finality; inh:parent; impl:interfacesList; className:identifier; con:content}

type classTree = ClassTree of classTreeMap
|Empty
