open Types
open PrintBlock

(*functions to print the different modifiers *)
let printVisibility vis = match vis with
|Public -> print_string "visibility:public\n"
|Private -> print_string "visibility:private\n"
|Protected -> print_string "visibility:protected\n"
|Private_Package -> print_string "visibility:private package\n"

let printObjectType obj = match obj with 
|Interface -> print_string "Type : Interface\n"
|Class -> print_string "Type : Class\n"
|Enum -> print_string "Type : Enum\n"

let printAbstraction abs= match abs with
|Abstract -> print_string "abstraction:abstract\n"
|Concrete -> print_string "abstraction:concrete\n"

let printFinality fin = match fin with
|Final -> print_string "finality:final\n"
|Extendable -> print_string "finality:extendable\n"

let printStaticity sta = match sta with 
|Static -> print_string "staticity:static\n"
|NonStatic -> print_string "staticity:Non static\n"

(*function to print the class name *)
let printIdentifier iden = match iden with
|Identifier(identName) -> print_string (identName^"\n")


(*function to print the class parent name*)
let printParent parent = match parent with
|Some(parentName) -> print_string ("parentName:"^parentName^"\n")
|None -> print_string ("parent: No parent\n")

let rec printExceptions exceptions = match exceptions with 
Some(a::t) -> print_string "exceptions : " ; printIdentifier a; printExceptions (Some(t));
|Some([]) -> print_string "End exceptions\n"
|None -> print_string("No exception\n")


let printArgument argument = match argument with 
| {argType=argType; argName=argName}->printIdentifier argType; printIdentifier argName

let rec printArguments arguments = match arguments with
Some(a::t) -> print_string "arguments : " ; printArgument a; printArguments (Some(t));
|Some([]) -> print_string "End arguments\n"
|None -> print_string("No argument\n")

let printModifier modifier = match modifier with 
|Visibility vis -> printVisibility vis 
|Abstraction abs -> printAbstraction abs
|Finality fin -> printFinality fin
|Synchronization syn  -> print_string "Synchronized\n"
|Nativity nat -> print_string "Native\n"
|StrictFpity str -> print_string "StrictFp\n"
|Staticity sta -> print_string "Static\n"

let rec printModifiers modifiers = match modifiers with 
Some(a::t) -> print_string "modifier : " ; printModifier a; printModifiers (Some(t));
|Some([]) -> print_string "End modifiers\n"
|None -> print_string("No modifier\n")



let rec printParameter param = match param with 
|{name=paramName;extends=Some(parentName);super=None} -> printIdentifier paramName; print_string "extends:";printParameter parentName;
|{name=paramName;extends=None;super=Some(childName)} -> printIdentifier paramName; print_string "super:";printParameter childName;
|{name=paramName;extends=None;super=None} -> printIdentifier paramName 

(*function to print the parameters of the class or interface*)
let rec printParameters params = match params with
Some(a::t) -> print_string "params : " ; printParameter a; printParameters (Some(t));
|Some([]) -> print_string "End params\n"
|None -> print_string("No params\n")

let rec printInterfaces interfaces = match interfaces with
|Some(a::t) -> print_string "Interface : " ; printIdentifier a; printInterfaces (Some(t));
|Some([]) -> print_string "End Interfaces\n"
|None -> print_string("No interface\n")


let printClassContentTree tree = match tree with
| Initializer ({iniType=iniType;con=block}) -> print_string "Initializer : "; printStaticity iniType; printAST block 
| MethodTree ({parameters=parameterList; modif=modifiersMethod; returnType=returnType; name=methodName; args=arguments; thr=exceptionList; con=block }) ->
	printParameters parameterList; printModifiers modifiersMethod; printIdentifier returnType; printIdentifier methodName; printArguments arguments; printExceptions exceptionList;
	printAST block
| Empty -> print_string "Error in the method declaration"

let rec printCon content = match content with 
Some(a::t) -> print_string "classContent : " ; printClassContentTree a; printCon (Some(t));
|Some([]) -> print_string "End classContent\n"
|None -> print_string("No classContent\n")

let printTree tree = match tree with
| ClassTree({objectType=obj;modif=modifiersObject; inh=parent; impl=interfaces; parameters=params; className=identifier; con=content}) ->
	printObjectType obj; printModifiers modifiersObject; printParameters params; printParent parent; printInterfaces interfaces; printIdentifier identifier; printCon content
| InterfaceTree({objectType= obj;modif=modifiersObject;interfaceName=interfaceName;parameters=params;inh=parent;con=content}) ->
	printObjectType obj; printModifiers modifiersObject; printIdentifier interfaceName; printParameters params; printInterfaces parent;
| EnumTree	({objectType=obj;modif=modifiersObject;enumName=enumName;inh=parent;con=content}) ->
	printObjectType obj; printModifiers modifiersObject; printIdentifier enumName; printInterfaces parent



	
