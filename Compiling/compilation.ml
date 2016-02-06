open AST
open Type
open Lexing
open Location 
(* Something to help with matching*)
type test = {name:string}

let test lol str = match lol with
|{name=var} when var = str  -> print_string str;
|_ -> print_string "egzegzegze"

(*------------------------------------------------*)

type tableMethod = astmethod list

and descriptorClass = {name: string; methods: string list ;attributes: astattribute list}

and descriptorObject = {objectName:string; attributes: astattribute list}

and tableDescriptorClass= string*descriptorClass  

and data = {dcs: descriptorClass list; tm : tableMethod ; dos: descriptorObject list  }

(** PRINTING FUNCTIONS **)

let rec printAttributes astAttributes = match astAttributes with 
| a::t -> print_string (a.aname^" "); printAttributes t
| [] -> print_string "End Attributes\n"

let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l

let rec printDescriptorClass descriptorClass = match descriptorClass with 
| {name= name; methods= methods ;attributes= astattributes }::t -> 
	print_string ("class :"^name^"\n"); print_list methods; print_string "End Methods \n"; printAttributes astattributes; print_string ("End Class :"^name^"\n"); printDescriptorClass t
| [] -> print_string "End Descriptors Class\n"

let rec printTableMethod tableMethod = match tableMethod with 
| a::t -> print_string "MethodFound : ";print_string (a.mname^"\n"); printTableMethod t
| [] -> print_string "End Table Methods\n"

let rec printDescriptorObject descriptorsObject = match descriptorsObject with 
| a::t -> print_string (a.objectName^" "); printAttributes a.attributes ;printDescriptorObject t
| [] -> print_string "End Objects\n"

let printData data = match data with 
|{dcs= descriptorsClass ; tm = tableMethod ; dos= descriptorsObject  } -> printTableMethod tableMethod; printDescriptorClass descriptorsClass; printDescriptorObject descriptorsObject 
(****************************************************)




let classZone = []

let tableMethods = []

let buildData dcs tm dos = {dcs=dcs; tm=tm; dos=dos}

let rec searchTypeList className typeList = match typeList with 
	| {modifiers = modifiers; id = id; info = info}::t -> print_string ("searching..."^className^"\n"); if className = id then {modifiers = modifiers; id = id; info = info}  else searchTypeList className t
	| [] -> print_string "ERROR Class not found\n"; exit 1
	
let rec searchForAttributes className dcs = match dcs with 
	| {name=name; methods=methods; attributes=astattributes}::t -> print_string ("searching attributes..."^className^"\n"); if className = name then astattributes else searchForAttributes className t
	| [] -> print_string "ERROR Class not found\n"; exit 1

let findClass ast className = match ast with 
| {package = pack;type_list = typeList} -> searchTypeList className typeList

let rec notCompiled className dcs = match dcs with 
| {name= name; methods= methods ;attributes=astattributes }::t -> if className = name then false else notCompiled className t
| [] -> true

let rec buildDescriptorClass tableMethod regexpId listMethodsClass = match tableMethod with
| {mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t -> 
	if Str.string_match regexpId mname 0 then mname::(buildDescriptorClass t regexpId listMethodsClass) else buildDescriptorClass t regexpId listMethodsClass
| [] -> listMethodsClass

let rec addMethods className methods tableMethods = match methods with 
| {mmodifiers = modifiers;mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t -> 
	print_string ("adding method "^mname^"\n"); 
	let newTableMethod = {mmodifiers = modifiers;mname = className^"_"^mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}:: tableMethods in addMethods className t newTableMethod
| [] -> tableMethods

let rec addAttributes  listParentAttributes astattributeList = match listParentAttributes with 
| {amodifiers = modifiers;aname =aName;atype =aType ;adefault=expression;(*      aloc : Location.t;*)}::t ->
	let newAstAttribute = {amodifiers=modifiers; aname=aName; atype=aType; adefault=expression; (*      aloc : Location.t;*)}::astattributeList in 
	addAttributes t newAstAttribute
| [] -> astattributeList

let rec compile astTyped info id data= match info with 
| {cparent=ref_type; cattributes=astattributeList; cinits=initialList; cconsts=astconstList; cmethods=astmethodList; cloc=location;} when ref_type=object_type ->
 	if notCompiled id data.dcs then
 		begin
		let newTableMethod = addMethods id astmethodList data.tm in
		let methods= buildDescriptorClass newTableMethod (Str.regexp_string (id^"_")) [] in
		let newAttributeListClass = addAttributes (searchForAttributes ref_type.tid data.dcs) astattributeList in
		let descriptorClass = {name=id; methods=methods; attributes = newAttributeListClass} in 
		buildData (descriptorClass::data.dcs) newTableMethod data.dos
		end
	else
		data
	
| {cparent = ref_type; cattributes = astattributeList;cinits = initialList; cconsts = astconstList; cmethods = astmethodList; cloc = location;} when ref_type!=object_type -> 
	if notCompiled ref_type.tid data.dcs then 
		begin
		let newData = compile astTyped (findClass astTyped ref_type.tid).info ref_type.tid data in 
		let newTableMethod = addMethods id astmethodList newData.tm in
		let newAttributeListClass = addAttributes (searchForAttributes ref_type.tid newData.dcs) astattributeList in
		let descriptorClass = {name=id; methods=buildDescriptorClass newTableMethod (Str.regexp_string (id^"_")) []; attributes = newAttributeListClass} in
		buildData (descriptorClass::newData.dcs) newTableMethod newData.dos
		end	
	else
		begin
		let newTableMethod = addMethods id astmethodList data.tm in
		let descriptorClass = {name=id; methods=buildDescriptorClass newTableMethod (Str.regexp_string (id^"_")) []; attributes = addAttributes (searchForAttributes ref_type.tid data.dcs) astattributeList} in
		buildData (descriptorClass::data.dcs) newTableMethod data.dos
		end
let rec typeListWalk astTyped type_list data = match type_list with 
| {modifiers = modifiers; id = id; info = info} :: t -> typeListWalk astTyped t (compile astTyped info id data)
| [] -> printData data


(*Class Object from java.lang package*)

let objectClass = {name="Object"; methods=["equals"];attributes= []}

let object_equals obj1 obj2 = match obj1 with
| obj1 when obj1==obj2 -> true
| obj1 when obj1!=obj2 -> false

let initData =  {dcs= [objectClass]; tm = [] ; dos= []  }

let treeWalk astTyped = match astTyped with
| {package = pack;type_list = typeList} -> typeListWalk astTyped typeList initData




