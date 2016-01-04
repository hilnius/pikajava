open Types

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

(*function to print the class name *)
let printIdentifier iden = match iden with
|Identifier(identName) -> print_string (identName^"\n")

(*function to print the class parent name*)
let printParent parent = match parent with
|Some(parentName) -> print_string ("parentName:"^parentName^"\n")
|None -> print_string ("parent: No parent\n")

let printParameter param = match param with 
{name=paramName;extends=parentName} -> printIdentifier paramName; printParent parentName

(*function to print the parameters of the class or interface*)
let rec printParameters params = match params with
Some(a::t) -> print_string "params : " ; printParameter a; printParameters (Some(t));
|Some([]) -> print_string "End params\n"
|None -> print_string("No params\n")

let rec printInterfaces interfaces = match interfaces with
|Some(a::t) -> print_string "Interface : " ; printIdentifier a; printInterfaces (Some(t));
|Some([]) -> print_string "End Interfaces\n"
|None -> print_string("No interface\n")

let printTree tree = match tree with
| ClassTree({objectType= obj;vis=vis; abs=abs; fin=fin; inh=parent; impl=interfaces; parameters=params; className=identifier; con=content}) ->
	printObjectType obj; printVisibility vis; printAbstraction abs; printFinality fin;printParameters params; printParent parent; printInterfaces interfaces; printIdentifier identifier
| InterfaceTree({objectType= obj;vis=vis;interfaceName=interfaceName;parameters=params;inh=parent;con=content}) ->
	printObjectType obj; printVisibility vis; printIdentifier interfaceName; printParameters params; printInterfaces parent;
| EnumTree	({objectType= obj;vis=vis;enumName=enumName;inh=parent;con=content}) ->
	printObjectType obj; printVisibility vis; printIdentifier enumName; printInterfaces parent;
	
