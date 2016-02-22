open Compilation
open AST
open Type
(* TO DO *)
(*execReadyData = {dcs: evaluatedDescriptorClass list; tm : tableMethod ; dos: descriptorObject list}
type scopedData = {data:execReadyData;currentScope:int}*)  

type this = {thisName:string;thisType:Type.t;thisScope:int}
type scopedData = {data:data;currentScope:int;currentObject:descriptorObject;stack:this list}

type astMethodWithType = astmethod * Type.t 

let changeCurrentObject scopedData currentObject = match scopedData with
| {data=data; currentScope=scope; currentObject=oldObject;stack=thisList} ->  {data=data; currentScope=scope; currentObject=currentObject;stack=thisList}


let changeElementToOption element = Some element
let changeListToOptionList listToModify = List.map changeElementToOption listToModify

let constructNull currentScope name = {objectName=name; attributes=[]; objectValue=Compilation.Null; scope=currentScope}

let rec getMain tableMethod = match tableMethod with 
|{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t->
if (( List.mem AST.Public modifiers) && (List.mem AST.Static modifiers) && (List.length modifiers = 2) ) && (mname = "identifier$main") && (mreturntype = Type.Void) (* TEST ARGUMENT *) then
	{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}
else getMain t
|[] -> print_string "No Main!\n"; exit 0

(* Look for method prefixed *)
let rec getConstructor eType tableMethod methodName = match tableMethod with 
|{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t->
if (mname = methodName) then
	({mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)},eType)
else getConstructor eType t methodName
|[] -> print_string ("No such constructor : "^methodName^"\n"); exit 1

(* Look for method prefixed *)
let rec getMethod callerType tableMethod methodName = let newRefType callerTypeT =  
	match callerTypeT with 
	(*| Void -> 
  	| Array of t * int
  	| Primitive of primitive*)
  	| Ref (aRefType) -> aRefType
	in
	
	let refType = newRefType callerType in 
	match tableMethod with 
	|{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (* mloc : Location.t;*)}::t->
		if (mname = (refType.tid^"$"^methodName)) then
			({mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (* mloc : Location.t;*)},callerType)
		else getMethod callerType t methodName
	|[] -> print_string ("No such method : "^methodName^"\n"); exit 1


let rec findMaxScope aListOfObject highestScope objectDescriptor= match aListOfObject with 
| a::t -> if a.scope > highestScope then findMaxScope t a.scope a else findMaxScope t highestScope objectDescriptor
| [] -> objectDescriptor


(*TODO HANDLE SCOPE IN IF *)
let rec findObjectInData descriptorsObject anObjectName listMatched = match descriptorsObject,listMatched with
|a::t , _-> if a.objectName= anObjectName  then findObjectInData t anObjectName (a::listMatched) else findObjectInData t anObjectName listMatched
|[],[] -> print_string (anObjectName^ "IS NOT DECLARED");exit 1;
| [], a::t -> findMaxScope (a::t) 0 (constructNull 0 "")

let getIntValue descriptorObject = match descriptorObject.objectValue with 
| Int(a) -> a
| Null -> 0


let setValue descriptorObject newValue = match descriptorObject with 
|{objectName=aName; attributes=aList; objectValue=aValue; scope=aScope} -> {objectName=aName; attributes=aList; objectValue=newValue; scope=aScope} 

let rec notEquals anObject aSecondObject =
(anObject.objectName != aSecondObject.objectName) ||  (anObject.scope != aSecondObject.scope)

let filter f aList = 
	let rec newListFilter inputFunc inputList listToReturn = 
		match inputList with 
		| a::t -> 
			if inputFunc a then newListFilter inputFunc t (a::listToReturn) else newListFilter inputFunc t listToReturn
		|[] -> listToReturn 
	in
	newListFilter f aList []
	
let changeName newName objectDescriptor = match objectDescriptor with
{objectName = oldName; attributes = attributes; objectValue = value; scope = scope} -> {objectName = newName; attributes = attributes; objectValue = value; scope=scope}

let rec deleteObject objectToDelete scopedData = 
	let newfunc = (notEquals objectToDelete) in
	let newDos = filter newfunc scopedData.data.dos  in
	{data=(buildData scopedData.data.dcs scopedData.data.tm newDos); currentScope = scopedData.currentScope; currentObject = scopedData.currentObject; stack=scopedData.stack} 
	
let incrementScope scopedData = match scopedData with 
{data=data;currentScope = currentScope;currentObject=currentObject; stack=thisList} -> {data=data;currentScope = currentScope+1;currentObject=currentObject; stack=thisList}

let deleteVariablesInScope descriptorsObject scopeToErase = 
	let rec deleteTargetedScope objectList scopeTargeted listUpdated = 
		match objectList with
		| a::t ->
			if a.scope = scopeTargeted then  begin Compilation.printOneDescriptorObject a; deleteTargetedScope t scopeTargeted listUpdated end
			else deleteTargetedScope t scopeTargeted (a::listUpdated)
		|[] -> listUpdated
	in	
	deleteTargetedScope descriptorsObject scopeToErase []
	
let decrementScope scopedData = match scopedData with 
|{data=data;currentScope = currentScope;currentObject=currentObject; stack=(a::t)} -> 
	let updatedOjectDescriptor =  deleteVariablesInScope data.dos currentScope in
	{data=(buildData data.dcs data.tm updatedOjectDescriptor);currentScope = currentScope-1;currentObject=currentObject; stack=t}
|{data=data;currentScope = currentScope;currentObject=currentObject; stack=[]} -> 
	let updatedOjectDescriptor =  deleteVariablesInScope data.dos currentScope in
	{data=(buildData data.dcs data.tm updatedOjectDescriptor);currentScope = currentScope-1;currentObject=currentObject; stack=[]}

let evaluateNameString anObjectName scopedData = match anObjectName with 
|"this" -> 
	if scopedData.stack = [] then 
		begin
			print_string "UNEXPECTED THIS";
			exit 1
		end
	else
		changeCurrentObject scopedData (findObjectInData scopedData.data.dos (List.hd scopedData.stack).thisName [])
| _ -> changeCurrentObject scopedData (findObjectInData scopedData.data.dos anObjectName [])


let rec findAttribute attributes attributeName = match attributes with 
| a::t ->
	if a.objectName = attributeName then a 
	else findAttribute t attributeName
| [] -> print_string ("ATTRIBUTE "^attributeName^" NOT FOUND\n"); exit 1
let deconstructExpressionType expression = match expression with 
{edesc = expression_desc;(* eloc : Location.t; *)etype = Some etype;} ->  etype


let rec findClassDescriptor className descriptorsClass = match descriptorsClass with 
| {classType= aType; methods= methods ;attributes= astattributes }::t ->
	print_string className;
	let result =  match aType with 
 	(*| Array of t * int*)
	(*| Primitive(primitive) -> match primitive with *)
	| Ref(refType) ->
		if refType.tid=className then {classType= aType; methods= methods ;attributes= astattributes }
		else findClassDescriptor className t
	in 
	result
| [] -> print_string "Class not found!\n"; exit 1



let rec initializeAttributes className scopedData = 
	let classDescriptor = findClassDescriptor className scopedData.data.dcs in
	let objectToCreate = {objectName=""; attributes= []; objectValue = Compilation.Instanciated; scope=scopedData.currentScope} in
	let rec evaluateAttributes scope attributes objectToFill = 
		match attributes,objectToFill with 
		|	{
				amodifiers = modifiers ;
				aname = aName;
				atype = aType;
				adefault = Some expression ;
				  (*      aloc = Location.t;*)
			}::t,
			{
				objectName = ""; 
				attributes = oldAttributes;
				objectValue = Compilation.Instanciated;
				scope = scopeValue;
			} ->
				let newScope = evaluateExpression expression scopedData	in
				let newObjectToFill = {objectName=""; attributes = oldAttributes@[(changeName aName newScope.currentObject)]; objectValue = Compilation.Instanciated; scope=scope.currentScope} in	
				evaluateAttributes newScope t newObjectToFill
		|	{
				amodifiers = modifiers ;
				aname = aName;
				atype = aType;
				adefault = None;
				  (*      aloc = Location.t;*)
			}::t,
			{
				objectName = ""; 
				attributes = oldAttributes;
				objectValue = Compilation.Instanciated;
				scope = scopeValue
			} ->
				let newObjectToFill = {objectName=""; attributes= oldAttributes@[constructNull scope.currentScope aName]; objectValue = Compilation.Instanciated; scope = scope.currentScope} in	
				evaluateAttributes scope t newObjectToFill
		| [], _ -> changeCurrentObject scope objectToFill
	in	
	evaluateAttributes scopedData classDescriptor.attributes objectToCreate
		
			
and evaluateExpression expression scopedData = match expression with 
|	{
      	edesc = expression_desc;
		(* eloc : Location.t; *)
      etype = eType;
    } -> 
    	match expression_desc with
    	| Val(Int(i)) -> changeCurrentObject scopedData {objectName=""; attributes=[]; objectValue=Int(int_of_string i); scope=scopedData.currentScope}
    	| Val(Boolean(b)) -> changeCurrentObject scopedData {objectName=""; attributes=[]; objectValue=Bool(b); scope=scopedData.currentScope}
		| New(None,className, expressions) ->
			let finalClassName = List.nth className ((List.length className) - 1) in
			let scopedWithInitialized = initializeAttributes finalClassName scopedData  in
			executeMethod (getConstructor (deconstructExpressionType expression) scopedWithInitialized.data.tm (finalClassName^"$"^finalClassName)) scopedWithInitialized (changeListToOptionList expressions)
		| New(Some outer,className, expressions) ->
			let finalClassName = List.nth className ((List.length className) - 1) in
			let scopedWithInitialized = initializeAttributes finalClassName scopedData  in
			executeMethod (getConstructor (deconstructExpressionType expression) scopedWithInitialized.data.tm (finalClassName^"$"^finalClassName)) scopedWithInitialized (changeListToOptionList expressions)	
		| Name (anObjectName) -> evaluateNameString anObjectName scopedData
		| Op(expression1,operator,expression2) ->
			let evaluatedExpression1 = evaluateExpression expression1 scopedData in 
			let evaluatedExpression2 = evaluateExpression expression2 evaluatedExpression1 in 
			executeInfixOp evaluatedExpression1 operator evaluatedExpression2 evaluatedExpression2
		| AssignExp (expression1,assignOperator,expression2) -> executeAssignExp expression1 assignOperator expression2 scopedData
		| Call (Some callerExpression, methodName, arguments) -> 				
			if methodName = "println" then
				if List.length arguments = 0 then 
					begin
						print_string "\n";
						scopedData	
					end						
				else
					begin
						let evaluateArgument argumentsList = match argumentsList with 
						| a::[] -> evaluateExpression a scopedData 
						in
						let printArg arg = match arg with 
						| {objectName=aName; attributes=attributes; objectValue=Int(i); scope=currentScope} -> print_string ((string_of_int i)^"\n")
						| {objectName=aName; attributes=attributes; objectValue=Bool(b); scope=currentScope} -> print_string ((string_of_bool b)^"\n")
						| {objectName=aName; attributes=attributes; objectValue=String(s); scope=currentScope} ->  print_string (s^"\n")
						in
						let argument=evaluateArgument arguments	in 
						printArg argument.currentObject;
						argument
					end	
			else 
				begin		
					let callerType = deconstructExpressionType callerExpression in
					let callerScope = evaluateExpression callerExpression scopedData in
					executeMethod (getMethod callerType scopedData.data.tm methodName ) callerScope (changeListToOptionList arguments)
				end
		| Call (None, methodName, arguments) -> 
			let callerType = (List.hd  scopedData.stack).thisType in 
			executeMethod (getMethod callerType scopedData.data.tm methodName ) scopedData (changeListToOptionList arguments)
		| Attr (expression, attributeName) -> 
			let objectExpression = evaluateExpression expression scopedData in
			changeCurrentObject objectExpression (findAttribute scopedData.currentObject.attributes attributeName)

			
				(* 
		
		| If ( leftExpression, rightExpression,  elseExpression) ->
		  | ArrayInit of expression list
		  | Post of expression * postfix_op
		  | CondOp of expression * expression * expression
		  | Cast of expression * expression
		  | Instanceof of expression * expression*)
and updateObject descriptorObject scopedData =
	let objectName = descriptorObject.objectName in
	let deletedScope = deleteObject descriptorObject scopedData in
	addObject descriptorObject.scope objectName deletedScope 
	 	  
		  
and executeAssignExp expression1 assignOperator expression2 scopedData =
	let evaluatedExpression1 = evaluateExpression expression1 scopedData in 
	let evaluatedExpression2 = evaluateExpression expression2 evaluatedExpression1 in
	let currentObjectScope = evaluatedExpression1.currentObject.scope in
	let newScope = deleteObject evaluatedExpression1.currentObject scopedData in
	match assignOperator with 
	|Ass_add -> addObject currentObjectScope evaluatedExpression1.currentObject.objectName (executeInfixOp evaluatedExpression1 Op_add evaluatedExpression2 newScope)
  
and executeInfixOp evaluatedExpression1 operator evaluatedExpression2 scopedData =
	let returnObject = {objectName=""; attributes=[]; objectValue=Compilation.Null; scope=scopedData.currentScope}in 
	match operator with
	|Op_add -> changeCurrentObject scopedData (setValue returnObject (Int((getIntValue evaluatedExpression1.currentObject)  + (getIntValue evaluatedExpression2.currentObject)))) 
    |Op_eq -> changeCurrentObject scopedData (setValue returnObject (Bool((getIntValue evaluatedExpression1.currentObject) = (getIntValue evaluatedExpression2.currentObject)))) 
and addObject currentObjectScope name scopedData = match scopedData with 
| {data={dcs= descriptorsClass ; tm = tableMethod ; dos= descriptorObject  };currentScope=currentScope; currentObject=currentObject; stack=thisList} ->
	let objectNamed = {objectName=name; attributes=currentObject.attributes; objectValue=currentObject.objectValue; scope=currentObjectScope} in 
	{data={dcs= descriptorsClass ; tm = tableMethod ; dos= (objectNamed::descriptorObject)};currentScope=currentScope; currentObject=objectNamed; stack=thisList}

and executeStatement statement scopedData = match statement with 
| VarDecl ((aType, name, Some(expression))::t) ->
	let resultExpression = (evaluateExpression expression scopedData) in
	(addObject scopedData.currentScope name resultExpression, false)
| VarDecl ((aType, name, None) ::t) -> (addObject scopedData.currentScope name (changeCurrentObject scopedData (constructNull scopedData.currentScope "")), false)
| Return ( Some expression) -> ((evaluateExpression expression scopedData ), true)
| Return ( None ) -> (scopedData, true)
| If (ifExpression, thenStatement, Some elseStatement ) ->
	let scopedDataIfBooleanEvaluated = evaluateExpression ifExpression scopedData in
		if scopedDataIfBooleanEvaluated.currentObject.objectValue = Bool(true) then
			begin
			executeStatement thenStatement scopedDataIfBooleanEvaluated
			end
		else 
			executeStatement elseStatement scopedDataIfBooleanEvaluated
|If (ifExpression, thenStatement, None ) ->
	let scopedDataIfBooleanEvaluated = evaluateExpression ifExpression scopedData in
		if scopedDataIfBooleanEvaluated.currentObject.objectValue = Bool(true) then
			begin
			executeStatement thenStatement scopedDataIfBooleanEvaluated
			end
		else 
			(scopedData,false)				
| Block (statements) ->
	let incrementedScope = incrementScope scopedData in
	let scopeAfterStatements = executeStatements statements incrementedScope in
	(decrementScope scopeAfterStatements, false)
| While (expression, statement)->
	let rec whileExecution aCondition aStatement scopedDataExecution= 
		let scopeAfterCondition = evaluateExpression aCondition scopedDataExecution in
			if scopeAfterCondition.currentObject.objectValue = Bool(true) then
				begin
					let executedStatement = fst(executeStatement aStatement scopeAfterCondition) in
					whileExecution aCondition aStatement executedStatement	
				end
			else	
			(scopedDataExecution,false)
	in	
	whileExecution expression statement scopedData	
(* 
| Nop -> 

| For ((Type.t , string,expression option) list ,expression option , expression list, statement) ->

| Throw ( expression) ->
| Try (statement list, (argument * statement list) list, statement list)
*)
| Expr (expression) -> ((evaluateExpression expression scopedData), false)
 
and executeStatements statements scopedData = match statements with 
| a::t ->
	let resultStatement = executeStatement a scopedData in
	if snd resultStatement then fst resultStatement
	else executeStatements t (fst resultStatement)
| [] -> scopedData
 
and evaluateAndAddToScopeArgs argumentsToEvaluate methodArgumentsProt scopedData = 
	if List.length argumentsToEvaluate = List.length methodArgumentsProt then
	begin
		match argumentsToEvaluate, methodArgumentsProt with
		| argExpr::t, argProt::m -> evaluateAndAddToScopeArgs t m (fst (executeStatement (VarDecl([argProt.ptype, argProt.pident, argExpr])) scopedData))
		| [], _ -> scopedData
	end
	else 
	begin
		print_string "Wrong number of arguments!\n";
		exit 1
	end

and addNewThis aType scopedData = match scopedData with 
{data=data;currentScope = currentScope;currentObject=currentObject; stack=thisList}-> {data=data;currentScope = currentScope;currentObject=currentObject; stack={thisName=currentObject.objectName;thisType=aType;thisScope=scopedData.currentScope}::thisList}

and executeMethod aMethod scopedData argumentsToEvaluate = match aMethod with 
|({mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)},callerType)->
	(*if currentObject.objectValue = Compilation.Null
	then RAISE EXCPETION
	else*)
	let incrementedScope = incrementScope scopedData in
	if (not (List.mem AST.Static modifiers)) then
		begin
			let updatedScope = addNewThis callerType incrementedScope in
			let excutedMethodScope = executeStatements statements (evaluateAndAddToScopeArgs argumentsToEvaluate arguments updatedScope)  in
			decrementScope excutedMethodScope
		end
	else
		begin 
			let updatedScope = incrementedScope in
			let excutedMethodScope = executeStatements statements (evaluateAndAddToScopeArgs argumentsToEvaluate arguments updatedScope)  in
			decrementScope excutedMethodScope 
		end







 	
