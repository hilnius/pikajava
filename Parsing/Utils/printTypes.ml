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
|Identifier(className) -> print_string ("className:"^className^"\n")

(*function to print the class parent name*)
let printParent parent = match parent with
|Some(parentName) -> print_string ("parentName:"^parentName^"\n")
|None -> print_string ("parent: No parent\n")

let rec printInterfaces interfaces = match interfaces with
|Some(a::t) -> print_string "Interface : " ; printIdentifier a; printInterfaces (Some(t));
|Some([]) -> print_string "End Interfaces"
|None -> print_string("No interface\n")

let printTree tree = match tree with
| ClassTree({objectType= obj;vis=vis; abs=abs; fin=fin; inh=parent; impl=interfaces; className=identifier; con=content}) ->
	printObjectType obj; printVisibility vis; printAbstraction abs; printFinality fin; printParent parent; printInterfaces interfaces; printIdentifier identifier
| InterfaceTree({objectType= obj;vis=vis;interfaceName=interfaceName;inh=parent;con=content}) ->
	printObjectType obj; printVisibility vis; printIdentifier interfaceName; printInterfaces parent;
| EnumTree	({objectType= obj;vis=vis;enumName=enumName;inh=parent;con=content}) ->
	printObjectType obj; printVisibility vis; printIdentifier enumName; printInterfaces parent;
	
