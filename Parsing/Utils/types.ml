type visibility = 
	| PUBLIC   
  | PROTECTED
  | PRIVATE 
  | PACKAGE_PRIVATE
  
type abstraction = 
	| ABSTRACT
	| CONCRETE

type finality = 
	| FINAL
	| EXTENDABLE

type identifier = IDENTIFIER of string

type classListAttribute = 
	| CLASS_ATTRIBUTE of classAttribute
	| Empty							

(*TODO type classAttribute to be implemented*)
type classAttribute = Empty

type classListMethod = 
	| CLASS_METHOD of classMethod
	| Empty					

(*TODO type classMethod to be implemented*)
type classMethod = Empty

type content  = CONTENT of (classListAttribute * classListMethod )   



