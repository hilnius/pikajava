open Compilation
open AST

(* TO DO *)
(*execReadyData = {dcs: evaluatedDescriptorClass list; tm : tableMethod ; dos: descriptorObject list}
type scopedData = {data:execReadyData;currentScope:int}*)
type scopedData = {data:data;currentScope:int;currentObject:descriptorObject}

let changeCurrentObject scopedData currentObject = match scopedData with
| {data=data; currentScope=scope; currentObject=oldObject} ->  {data=data; currentScope=scope; currentObject=currentObject}


let changeElementToOption element = Some element
let changeListToOptionList listToModify = List.map changeElementToOption listToModify

let constructNull currentScope = {objectName=""; attributes=[]; objectValue=Compilation.Null; scope=currentScope}

let rec getMain tableMethod = match tableMethod with 
|{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t->
if (( List.mem AST.Public modifiers) && (List.mem AST.Static modifiers) && (List.length modifiers = 2) ) && (mname = "identifier$main") && (mreturntype = Type.Void) (* TEST ARGUMENT *) then
	{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}
else getMain t
|[] -> print_string "No Main!\n"; exit 0

(* Look for method prefixed *)
let rec getMethod tableMethod methodName = match tableMethod with 
|{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t->
if (mname = methodName) then
	{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}
else getMethod t methodName
|[] -> print_string ("No such method : "^methodName^"\n"); exit 1


let rec findMaxScope aListOfObject highestScope objectDescriptor= match aListOfObject with 
| a::t -> if a.scope > highestScope then findMaxScope t a.scope a else findMaxScope t highestScope objectDescriptor
| [] -> objectDescriptor

(*TODO HANDLE SCOPE IN IF *)
let rec findObjectInData descriptorsObject anObjectName listMatched = match descriptorsObject,listMatched with
|a::t , _-> if a.objectName= anObjectName  then findObjectInData t anObjectName (a::listMatched) else findObjectInData t anObjectName listMatched
|[],[] -> print_string (anObjectName^ "IS NOT DECLARED");exit 1;
| [], a::t -> findMaxScope (a::t) 0 (constructNull 0)

let getValue descriptorObject = match descriptorObject.objectValue with 
| Int(a) -> a

let setValue descriptorObject newValue = match descriptorObject with 
|{objectName=aName; attributes=aList; objectValue=aValue; scope=aScope} -> {objectName=aName; attributes=aList; objectValue=newValue; scope=aScope} 

let rec notEquals anObject aSecondObject =
print_string (anObject.objectName ^ (string_of_int anObject.scope));
print_string (aSecondObject.objectName ^ (string_of_int aSecondObject.scope));

print_string (string_of_bool((anObject.objectName != aSecondObject.objectName) ||  (anObject.scope != aSecondObject.scope)));
(anObject.objectName != aSecondObject.objectName) ||  (anObject.scope != aSecondObject.scope)

let filter f aList = 
	let rec newListFilter inputFunc inputList listToReturn = 
		match inputList with 
		| a::t -> 
			if inputFunc a then newListFilter inputFunc t (a::listToReturn) else newListFilter inputFunc t listToReturn
		|[] -> listToReturn 
	in
	newListFilter f aList []
	

let rec deleteObject objectToDelete scopedData = 
	print_string (string_of_int (List.length scopedData.data.dos));
	let newfunc = (notEquals objectToDelete) in
	let newDos = filter newfunc scopedData.data.dos  in
	print_string (string_of_int (List.length newDos));
	{data=(buildData scopedData.data.dcs scopedData.data.tm newDos); currentScope = scopedData.currentScope; currentObject = scopedData.currentObject} 
	

let rec evaluateExpression expression scopedData = match expression with 
|	{
      	edesc = expression_desc;
		(* eloc : Location.t; *)
      etype = eType;
    } -> 
    	match expression_desc with
    	| Val(Int(i)) -> changeCurrentObject scopedData {objectName=""; attributes=[]; objectValue=Int(int_of_string i); scope=1}
		| New(className, expressions) ->
			let finalClassName = List.nth className ((List.length className) - 1) in
			executeMethod (getMethod scopedData.data.tm (finalClassName^"$"^finalClassName)) scopedData (changeListToOptionList expressions)
		| Name (anObjectName) -> changeCurrentObject scopedData (findObjectInData scopedData.data.dos anObjectName [])
		| Op(expression1,operator,expression2) ->
			let evaluatedExpression1 = evaluateExpression expression1 scopedData in 
			let evaluatedExpression2 = evaluateExpression expression2 evaluatedExpression1 in 
			executeInfixOp evaluatedExpression1 operator evaluatedExpression2 evaluatedExpression2
		| AssignExp (expression1,assignOperator,expression2) -> executeAssignExp expression1 assignOperator expression2 scopedData
		(* 
		  | Call of expression * string * expression list
		  | Attr of expression * string
		  | If of expression * expression * expression
		  | ArrayInit of expression list
		  | Post of expression * postfix_op
		  | CondOp of expression * expression * expression
		  | Cast of expression * expression
		  | Instanceof of expression * expression*)
and executeAssignExp expression1 assignOperator expression2 scopedData =
	let evaluatedExpression1 = evaluateExpression expression1 scopedData in 
	let evaluatedExpression2 = evaluateExpression expression2 evaluatedExpression1 in
	let newScope = deleteObject evaluatedExpression1.currentObject scopedData in
	print_string (string_of_int (List.length newScope.data.dos));
	match assignOperator with 
	|Ass_add -> addObject evaluatedExpression1.currentObject.objectName (executeInfixOp evaluatedExpression1 Op_add evaluatedExpression2 newScope)
		  
and executeInfixOp evaluatedExpression1 operator evaluatedExpression2 scopedData =
	let returnObject = {objectName=""; attributes=[]; objectValue=Compilation.Null; scope=scopedData.currentScope}in 
	match operator with
	|Op_add -> changeCurrentObject scopedData (setValue returnObject (Int((getValue evaluatedExpression1.currentObject)  + (getValue evaluatedExpression2.currentObject)))) 
    
(* Return evaluated attributes*)
(*let rec evaluateAttributes classDescriptor attributesToFill= match classDescr with 
|{
      amodifiers = modifiers;
      aname = name;
      atype = atype;
      adefault = Some(expression);
      (*      aloc : Location.t;*)
    } ::t  -> evaluateAttributes  t putValueInAttributeList(evaluateExpression expression)
| [] -> scopedData
*)

(*let findClassDescriptor classType descriptorsClass = match descriptorsClass with 
| {classType= aType; methods= methods ;attributes= astattributes }::t ->
	if aType=classType then {classType= aType; methods= methods ;attributes= astattributes }
	else findClassDescriptor classType t 
| [] -> print_string "Class not found!\n"; exit 1*)

and addObject name scopedData = match scopedData with 
| {data={dcs= descriptorsClass ; tm = tableMethod ; dos= descriptorObject  };currentScope=currentScope; currentObject=currentObject} ->
	let objectNamed = {objectName=name; attributes=currentObject.attributes; objectValue=currentObject.objectValue; scope=currentObject.scope} in 
	{data={dcs= descriptorsClass ; tm = tableMethod ; dos= (objectNamed::descriptorObject)};currentScope=currentScope; currentObject=objectNamed}
 
and executeStatement statement scopedData = match statement with 
| VarDecl ((aType, name, Some(expression))::t) -> 
	let resultExpression = (evaluateExpression expression scopedData) in
	addObject name resultExpression
| VarDecl ((aType, name, None) ::t) -> addObject name (changeCurrentObject scopedData (constructNull scopedData.currentScope))
(*| Block (statements) -> 
| Nop -> 
| While (expression, statement)->
| For ((Type.t , string,expression option) list ,expression option , expression list, statement) ->
| If (expression, statement, statement option) ->
| Return ( expression option) ->
| Throw ( expression) ->
| Try (statement list, (argument * statement list) list, statement list)
*)
| Expr (expression) -> (evaluateExpression expression scopedData)
 
and executeStatements statements scopedData = match statements with 
| a::t -> executeStatements t (executeStatement a scopedData)
| [] -> scopedData
 
and evaluateAndAddToScopeArgs argumentsToEvaluate methodArgumentsProt scopedData = 
if List.length argumentsToEvaluate = List.length methodArgumentsProt then
begin
	match argumentsToEvaluate, methodArgumentsProt with
	| argExpr::t, argProt::m -> evaluateAndAddToScopeArgs t m (executeStatement (VarDecl([argProt.ptype, argProt.pident, argExpr])) scopedData)
	| [], _ -> scopedData
end
else 
begin
	print_string "Wrong number of arguments!\n";
	exit 1
end

and executeMethod aMethod scopedData argumentsToEvaluate = match aMethod with 
|{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}->
	executeStatements statements (evaluateAndAddToScopeArgs argumentsToEvaluate arguments scopedData)    

(*let rec initEvaluatedDescriptorClass descriptorsClass tableMethods evaluatedDescriptorsClass = match descriptorsClass with
| {classType=aType; methods=methods; attributes=attributes}::t -> List.iter evaluateAttributes attributes tableMethods*)






 	
