open Compilation
open AST
open Type
(* TO DO *)
(*execReadyData = {dcs: evaluatedDescriptorClass list; tm : tableMethod ; dos: descriptorObject list}
type scopedData = {data:execReadyData;currentScope:int}*)  
(* -2 => n'est pas utilisé par la suite. *)
type tempAttr = {isAttr:bool;expr:expression option;attributeName:string option}

type this = {thisId:int;thisType:Type.t;thisScope:int}
type scopedData = {data:data;currentScope:int;currentObject:descriptorObject;stack:this list}

type astMethodWithType = astmethod * Type.t 

let changeCurrentObject scopedData currentObject = match scopedData with
| {data=data; currentScope=scope; currentObject=oldObject;stack=thisList} ->  {data=data; currentScope=scope; currentObject=currentObject;stack=thisList}

let changeElementToNotOption element = match element with 
|Some element -> element
let changeElementToOption element = Some element
let changeListToOptionList listToModify = List.map changeElementToOption listToModify

let constructNull currentScope name = {objectId = 0; objectName=name; attributes=[]; objectValue=Compilation.Null; scope=currentScope}

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

let isRef descriptorObject = match descriptorObject.objectValue with 
| Reference(id) -> true
| _ -> false

let rec getRef descriptorObject scopedData = 
	match descriptorObject.objectValue with 
	| Reference(refId) -> getRef (findRefInData scopedData.data.dos refId) scopedData
	| _ -> descriptorObject
	
and findRefInData dos refId = 
	match dos with
	|a::t->if a.objectId= refId then a else findRefInData t refId
	|[] -> print_string ("Could not find descriptor with id "^(string_of_int refId)); exit 1


let setValue descriptorObject newValue = match descriptorObject with 
|{objectId=id;objectName=aName; attributes=aList; objectValue=aValue; scope=aScope} -> {objectId=id;objectName=aName; attributes=aList; objectValue=newValue; scope=aScope} 

let rec notEquals anObject aSecondObject =
anObject.objectId != aSecondObject.objectId

let filter f aList = 
	let rec newListFilter inputFunc inputList listToReturn = 
		match inputList with 
		| a::t -> 
			if inputFunc a then newListFilter inputFunc t (a::listToReturn) else newListFilter inputFunc t listToReturn
		|[] -> listToReturn 
	in
	newListFilter f aList []
	
let changeName newName objectDescriptor = match objectDescriptor with
{objectId=id;objectName = oldName; attributes = attributes; objectValue = value; scope = scope} -> {objectId=id;objectName = newName; attributes = attributes; objectValue = value; scope=scope}

let changeScope newScope objectDescriptor = match objectDescriptor with
{objectId=id;objectName = oldName; attributes = attributes; objectValue = value; scope = scope} -> {objectId=id;objectName = oldName; attributes = attributes; objectValue = value; scope=newScope}

(*Delete object from dos*)
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
			if a.scope = scopeTargeted then  begin (*Compilation.printOneDescriptorObject a;*) deleteTargetedScope ((changeScope (-2) a)::t) scopeTargeted listUpdated end
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
		changeCurrentObject scopedData (findRefInData scopedData.data.dos (List.hd scopedData.stack).thisId )
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
	let objectToCreate = {objectId=incrementId scopedData.data.dos; objectName=""; attributes= []; objectValue = Compilation.Instanciated; scope=scopedData.currentScope} in
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
				objectId=objectId;
				objectName = ""; 
				attributes = oldAttributes;
				objectValue = Compilation.Instanciated;
				scope = scopeValue;
			} ->
				let newScope = evaluateExpression expression scopedData	in
				let newObjectToFill = {objectId=objectToFill.objectId;objectName=""; attributes = oldAttributes@[(changeName aName (changeScope (-1) newScope.currentObject))]; objectValue = Compilation.Instanciated; scope=scope.currentScope} in	
				evaluateAttributes newScope t newObjectToFill
		|	{
				amodifiers = modifiers ;
				aname = aName;
				atype = aType;
				adefault = None;
				  (*      aloc = Location.t;*)
			}::t,
			{
				objectId=objectId;
				objectName = ""; 
				attributes = oldAttributes;
				objectValue = Compilation.Instanciated;
				scope = scopeValue
			} ->
				let newObjectToFill = {objectId=objectToFill.objectId;objectName=""; attributes= oldAttributes@[constructNull (-1) aName]; objectValue = Compilation.Instanciated; scope = scope.currentScope} in	
				evaluateAttributes scope t newObjectToFill
		| [], _ -> changeCurrentObject scope objectToFill
	in	
	evaluateAttributes scopedData classDescriptor.attributes objectToCreate
		
and dealWithAssignAttr expressionToCheck = match expressionToCheck with 
|{
	edesc = Attr (expression, attributeName);
	(* eloc : Location.t; *)
    etype = eType;
 } -> {isAttr = true; expr = Some expression; attributeName = Some attributeName}
|_ ->{isAttr = false; expr = None; attributeName = None}
			
and changeAllRef scopedRefToChange anObject = 
	 let objectList = scopedRefToChange.data.dos in 
	 let rec changeRef listObjects objectDes firstScope = match listObjects with 
	 |{objectId=objectId; objectName=objName; attributes=attributes; objectValue=Reference(refId); scope=scope}::t -> 
	 	if refId = objectDes.objectId then
	 		begin 
	 			let scopeWithDelete = deleteObject {objectId=objectId;objectName=objName; attributes=attributes; objectValue=Reference(refId); scope=scope} scopedRefToChange in
	 			let scopeAfterAssign = assignObject {objectId=objectId;objectName=objName; attributes=attributes; objectValue=Reference(refId); scope=scope} objectDes scopeWithDelete in
	 			changeRef t objectDes scopeAfterAssign
	 		end	
		else 
			changeRef t objectDes firstScope
	|{objectId=objectId; objectName=objName; attributes=attributes; objectValue=_; scope=scope}::t -> changeRef t objectDes firstScope	
	|[] -> firstScope
	in
	changeRef objectList anObject scopedRefToChange
	
and evaluateExpression expression scopedData = match expression with 
|	{
      	edesc = expression_desc;
		(* eloc : Location.t; *)
      etype = eType;
    } -> 
    	match expression_desc with
    	| Val(Int(i)) -> changeCurrentObject scopedData {objectId=(-2); objectName=""; attributes=[]; objectValue=Int(int_of_string i); scope=scopedData.currentScope}
    	| Val(Boolean(b)) ->  changeCurrentObject scopedData {objectId=(-2); objectName=""; attributes=[]; objectValue=Bool(b); scope=scopedData.currentScope}
    	| Val(Null) -> changeCurrentObject scopedData (constructNull scopedData.currentScope "")
    	| Val(String(s)) -> changeCurrentObject scopedData {objectId=(-2); objectName=""; attributes=[]; objectValue=String(s); scope=scopedData.currentScope}
		| New(None,className, expressions) ->
			let finalClassName = List.nth className ((List.length className) - 1) in
			let scopedWithInitialized = initializeAttributes finalClassName scopedData  in
			executeMethod (getConstructor (deconstructExpressionType expression) scopedWithInitialized.data.tm (finalClassName^"$"^finalClassName)) scopedWithInitialized (changeListToOptionList expressions)
		| New(Some outer,className, expressions) ->
			let finalClassName = List.nth className ((List.length className) - 1) in
			let scopedWithInitialized = initializeAttributes finalClassName scopedData  in
			executeMethod (getConstructor (deconstructExpressionType expression) scopedWithInitialized.data.tm (finalClassName^"$"^finalClassName)) scopedWithInitialized (changeListToOptionList expressions)	
		| Name (anObjectName) ->   evaluateNameString anObjectName scopedData
		| Op(expression1,operator,expression2) -> 
			let evaluatedExpression1 = evaluateExpression expression1 scopedData in 
			let evaluatedExpression2 = evaluateExpression expression2 evaluatedExpression1 in 
			executeInfixOp evaluatedExpression1 operator evaluatedExpression2 evaluatedExpression2
		| AssignExp (expression1,assignOperator,expression2) -> 
			let attributeWithOwner = dealWithAssignAttr expression1 in
			if attributeWithOwner.isAttr then
				begin
					let objectWithAttributeScope = evaluateExpression (changeElementToNotOption attributeWithOwner.expr) scopedData in
					let referencedObject = getRef objectWithAttributeScope.currentObject objectWithAttributeScope in
					let scopeWithAttribute = changeCurrentObject objectWithAttributeScope (findAttribute referencedObject.attributes (changeElementToNotOption attributeWithOwner.attributeName)) in
					let changedScope = changeCurrentObject scopeWithAttribute (changeScope (-1) scopeWithAttribute.currentObject) in
					let newAttributeScope = executeAssignExp changedScope assignOperator (evaluateExpression expression2 objectWithAttributeScope) in
					let scopedToRef = changeAttribute referencedObject (changeElementToNotOption attributeWithOwner.attributeName) newAttributeScope in
					scopedToRef
				end
			else
				begin
					let evaluatedExpression1 = evaluateExpression expression1 scopedData in 
					let evaluatedExpression2 = evaluateExpression expression2 evaluatedExpression1 in
					let scopedToRef = executeAssignExp evaluatedExpression1 assignOperator evaluatedExpression2 in
					scopedToRef
				end	
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
						| {objectId=aId; objectName=aName; attributes=attributes; objectValue=Int(i); scope=currentScope} -> print_string ((string_of_int i)^"\n")
						| {objectId=aId; objectName=aName; attributes=attributes; objectValue=Bool(b); scope=currentScope} -> print_string ((string_of_bool b)^"\n")
						| {objectId=aId; objectName=aName; attributes=attributes; objectValue=String(s); scope=currentScope} ->  print_string (s^"\n")
						| {objectId=aId; objectName=aName; attributes=attributes; objectValue=Null; scope=currentScope} ->  print_string ("null\n")
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
			if isRef objectExpression.currentObject then
				begin
					let objectDesToChange = getRef objectExpression.currentObject objectExpression in
					changeCurrentObject objectExpression (findAttribute objectDesToChange.attributes attributeName)
				end
			else
				changeCurrentObject objectExpression (findAttribute objectExpression.currentObject.attributes attributeName)
			
		(* THE CONDIONNAL EXPRESSION CONTAINS THE ASSIGNS WTF*)
		| CondOp ( conditionnalExpression, trueExpression,  falseExpression) -> 
			let resultExpression = evaluateExpression conditionnalExpression scopedData in

			if (resultExpression).currentObject.objectValue= Bool(true) then
				begin
					let a = evaluateExpression trueExpression resultExpression in 
					a
				end
			else
				begin
				let falseExp =  evaluateExpression falseExpression resultExpression in
				falseExp
				end
				(* 
		
		
		  | ArrayInit of expression list
		  | Post of expression * postfix_op
		  | If of expression * expression * expression
		  | Cast of expression * expression
		  | Instanceof of expression * expression*)
		  
and changeAttribute objectWithAttribute attributeName newAttributeScope = 
let rec newAttributesList objectToUpdateAttributes attributeToUpdate attributeScope newListOfAttributes = 
	match objectToUpdateAttributes with 
	| a::t -> 
		if a.objectName = attributeToUpdate then begin newAttributesList t attributeToUpdate attributeScope ((changeScope (-1) attributeScope.currentObject)::newListOfAttributes) end
		else newAttributesList t attributeToUpdate attributeScope (a::newListOfAttributes)
	| [] -> newListOfAttributes
in
let attributesList = newAttributesList objectWithAttribute.attributes attributeName newAttributeScope [] in
let objectToAdd = {objectId=(-2); objectName=objectWithAttribute.objectName; attributes=attributesList; objectValue=objectWithAttribute.objectValue; scope=objectWithAttribute.scope}
in
changeObjectDes objectWithAttribute objectToAdd newAttributeScope

and assignObject descriptorObject1 descriptorObject2 scopedData = match descriptorObject2.objectValue with
| Int(i) -> changeObjectDes descriptorObject1 descriptorObject2 scopedData 
| Bool(b) -> changeObjectDes descriptorObject1 descriptorObject2 scopedData
| String(str) -> changeObjectDes descriptorObject1 descriptorObject2 scopedData 
| Instanciated ->
	let reassignedScope = reassignObject descriptorObject1.scope descriptorObject1.objectName (changeCurrentObject scopedData (setValue descriptorObject2 (Reference(descriptorObject2.objectId)))) in
	reassignedScope
	
| Reference (idRef) ->
	reassignObject descriptorObject1.scope descriptorObject1.objectName (changeCurrentObject scopedData (setValue descriptorObject2 (Reference(descriptorObject2.objectId)))) 
| Null -> reassignObject descriptorObject1.scope descriptorObject1.objectName scopedData
	 	  
		  
and executeAssignExp evaluatedExpression1 assignOperator evaluatedExpression2 = 
(*let newScope = deleteObject evaluatedExpression1.currentObject evaluatedExpression2 in*)
match assignOperator with 
|Assign -> let scope = assignObject evaluatedExpression1.currentObject evaluatedExpression2.currentObject evaluatedExpression2 in scope
|Ass_add -> 
	let rightScope = (executeInfixOp evaluatedExpression1 Op_add evaluatedExpression2 evaluatedExpression2) in
	changeObjectDes evaluatedExpression1.currentObject rightScope.currentObject rightScope
  
and executeInfixOp evaluatedExpression1 operator evaluatedExpression2 scopedData =
	let returnObject = {objectId=(-1); objectName=""; attributes=[]; objectValue=Compilation.Null; scope=scopedData.currentScope}in 
	match operator with
	|Op_add -> changeCurrentObject scopedData (setValue returnObject (Int((getIntValue evaluatedExpression1.currentObject)  + (getIntValue evaluatedExpression2.currentObject)))) 
    |Op_eq -> changeCurrentObject scopedData (setValue returnObject (Bool((getIntValue evaluatedExpression1.currentObject) = (getIntValue evaluatedExpression2.currentObject))))
   
and changeScopeInDos objectScope objectName dos = 
	let rec findObjectInDosAndUpdateScope scope name objectsDescriptors newObjectDescriptors = match objectsDescriptors with 
	| a::t -> if a.objectName= name && a.scope = scope then [(changeScope (-2) a)]@ newObjectDescriptors@t else findObjectInDosAndUpdateScope scope name t (a::newObjectDescriptors)
	|[] -> print_string ("Unbound object "^(name)); exit 1     
    in
    let newDos = findObjectInDosAndUpdateScope objectScope objectName dos [] in 
    newDos
    
(* Add new object *)
and addObject objectName scopedData = match scopedData with 
| {data={dcs= descriptorsClass ; tm = tableMethod ; dos= descriptorObject  };currentScope=currentScope; currentObject=currentObject; stack=thisList} ->
	let objectNamed = {objectId=(incrementId descriptorObject); objectName=objectName; attributes=currentObject.attributes; objectValue=currentObject.objectValue; scope=currentScope} in 
	{data={dcs= descriptorsClass ; tm = tableMethod ; dos= (objectNamed::descriptorObject)};currentScope=currentScope; currentObject=objectNamed; stack=thisList}
	
(* Reassign object *)
and reassignObject objectScope objectName scopedData = match scopedData with 
| {data={dcs= descriptorsClass ; tm = tableMethod ; dos= descriptorObject  };currentScope=currentScope; currentObject=currentObject; stack=thisList} ->
	let objectNamed = {objectId=(incrementId descriptorObject); objectName=objectName; attributes=currentObject.attributes; objectValue=currentObject.objectValue; scope=objectScope} in
	let newDos = changeScopeInDos objectScope objectName descriptorObject in 
	{data={dcs= descriptorsClass ; tm = tableMethod ; dos= (objectNamed::newDos)};currentScope=currentScope; currentObject=objectNamed; stack=thisList}

(* Change only value of object*)
and changeObjectDes oldObjectTargetedDes finalObjectDes scopedDataToUpdate =
	let scopeWithDelete = deleteObject oldObjectTargetedDes scopedDataToUpdate in
	
	let updateTheRightDesObject oldObjectDes newObjectDes scopedData =		
		match scopedData with 
		| {data={dcs= descriptorsClass ; tm = tableMethod ; dos= descriptorObject  };currentScope=currentScope; currentObject=currentObject; stack=thisList} ->
			let finalObject = 
				{
					objectId=oldObjectDes.objectId;
					objectName=oldObjectDes.objectName;
					attributes=newObjectDes.attributes;
					objectValue=newObjectDes.objectValue;
					scope=oldObjectDes.scope
				} 
			in 
			{data={dcs= descriptorsClass ; tm = tableMethod ; dos= (finalObject::descriptorObject)};currentScope=currentScope; currentObject=finalObject; stack=thisList}
	in
	
	if isRef oldObjectTargetedDes then 
		begin
			let objectRefDes = getRef oldObjectTargetedDes scopedDataToUpdate in
			updateTheRightDesObject objectRefDes finalObjectDes scopeWithDelete
		end
	else
		updateTheRightDesObject oldObjectTargetedDes finalObjectDes scopeWithDelete

(* Deprecated*)
(*and addObjectAndRef currentObjectScope name scopedData = match scopedData with 
| {data={dcs= descriptorsClass ; tm = tableMethod ; dos= descriptorObject  };currentScope=currentScope; currentObject=currentObject; stack=thisList} ->
	match currentObject.objectValue with
	| Reference(refName, refScope) ->
		let objectNamed = {objectName=name; attributes=currentObject.attributes; objectValue=Reference(currentObject.id;currentObject.objectName, currentObject.scope); scope=currentObjectScope} in
		{data={dcs= descriptorsClass ; tm = tableMethod ; dos= (objectNamed::descriptorObject)};currentScope=currentScope; currentObject=objectNamed; stack=thisList}
	| Instanciated ->
		let objectNamed = {objectName=name; attributes=currentObject.attributes; objectValue=Reference(currentObject.id;currentObject.objectName, currentObject.scope); scope=currentObjectScope} in 
		{data={dcs= descriptorsClass ; tm = tableMethod ; dos= (objectNamed::descriptorObject)};currentScope=currentScope; currentObject=objectNamed; stack=thisList}
	| _ ->
		let objectNamed = {objectId=incrementId descriptorObject;objectName=name; attributes=currentObject.attributes; objectValue=currentObject.objectValue; scope=currentObjectScope} in 
		{data={dcs= descriptorsClass ; tm = tableMethod ; dos= (objectNamed::descriptorObject)};currentScope=currentScope; currentObject=objectNamed; stack=thisList}
*)

and incrementId objectsDescriptors = 
	let rec getMaxId dos maxId = match dos with
	| a::t ->
		if a.objectId > maxId then getMaxId t a.objectId
	 	else getMaxId t maxId
	| [] -> maxId
	in
	(getMaxId objectsDescriptors 0) + 1

 (*(List.length objectsDescriptors) + 1*)

and executeStatement statement scopedData = match statement with 
| VarDecl ((aType, name, Some(expression))::t) -> 
	let resultExpression = (evaluateExpression expression scopedData) in
	(*What is this? let scopeWithAddedObject = addObjectAndRef scopedData.currentScope name resultExpression in *)
	let scopeWithNewObject = addObject name resultExpression in
	executeStatement (VarDecl(t)) scopeWithNewObject
| VarDecl ([]) -> (scopedData,false)	
| VarDecl ((aType, name, None)::t) -> 
	if aType = Primitive(Int) then
		begin
			(* let scopeWithAddedObject = addObjectAndRef scopedData.currentScope name (changeCurrentObject scopedData {objectName=""; attributes=[]; objectValue=Int(0); scope=scopedData.currentScope}) in*)
			let scopeWithNewObject = addObject name (changeCurrentObject scopedData {objectId=(-2); objectName=""; attributes=[]; objectValue=Int(0); scope=scopedData.currentScope}) in
			executeStatement (VarDecl(t)) scopeWithNewObject
		end
	else
		begin
			(*let scopeWithAddedObject = addObjectAndRef scopedData.currentScope name (changeCurrentObject scopedData (constructNull scopedData.currentScope "")) in*)
			let scopeWithNewObject = addObject name (changeCurrentObject scopedData (constructNull scopedData.currentScope "")) in
			executeStatement (VarDecl(t)) scopeWithNewObject
		end
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
| Nop -> (scopedData,false) 	
| For (initialExpressions ,conditionnalExpression, incrementExpression, statement) ->  
	let scopeWithVarDecl = initializeLoop initialExpressions scopedData in
	executeLoop scopeWithVarDecl (changeElementToNotOption conditionnalExpression) incrementExpression statement
	
(* 


| For ((Type.t , string,expression option) list ,expression option , expression list, statement) ->

| Throw ( expression) ->
| Try (statement list, (argument * statement list) list, statement list)
*)
| Expr (expression) -> ((evaluateExpression expression scopedData), false)

and executeLoop scopeWithVarDecl conditionnalExpression incrementExpressions statement = 
let conditionnalEvaluatedScope = evaluateExpression conditionnalExpression scopeWithVarDecl in
	if conditionnalEvaluatedScope.currentObject.objectValue = Bool(true) then
		begin
			let afterStatementScope = executeStatement statement conditionnalEvaluatedScope in 
			let incrementedScope = incrementLoop incrementExpressions (fst afterStatementScope) in
			executeLoop incrementedScope conditionnalExpression incrementExpressions statement
		end
	else
		(scopeWithVarDecl,false)
		
and incrementLoop incrementExpressions scopedData = match incrementExpressions with 
|a ::t -> incrementLoop t (evaluateExpression a scopedData)
|[] -> scopedData 
 
and initializeLoop initialExpressions scopedData = match initialExpressions with 
|(aType , incrementName,Some expression)::t -> initializeLoop t (fst (executeStatement (VarDecl([((changeElementToNotOption aType), incrementName, Some(expression))])) scopedData))
|(aType , incrementName,None)::t -> initializeLoop t (fst (executeStatement (VarDecl([((changeElementToNotOption aType), incrementName, None)])) scopedData))
|[] -> scopedData
 
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
		| argExpr::t, argProt::m -> 
			let callerScope = evaluateExpression (changeElementToNotOption argExpr) scopedData in
			let newScope = changeCurrentObject callerScope (setValue callerScope.currentObject (Reference(callerScope.currentObject.objectId))) in 
		evaluateAndAddToScopeArgs t m (addObject argProt.pident newScope) 
		| [], _ -> scopedData
	end
	else 
	begin
		print_string "Wrong number of arguments!\n";
		exit 1
	end

and addNewThis aType scopedData = match scopedData with 
{data=data;currentScope = currentScope;currentObject=currentObject; stack=thisList}-> {data=data;currentScope = currentScope;currentObject=currentObject; stack={thisId=currentObject.objectId;thisType=aType;thisScope=scopedData.currentScope}::thisList}

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







 	
