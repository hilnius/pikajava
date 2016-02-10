open AST
open Type
open Lexing
open Location 

type tableMethod = astmethod list

and descriptorClass = {classType: Type.t; methods: string list ;attributes: astattribute list}

and descriptorObject = {objectName:string; attributes: descriptorObject list; objectValue: attributeValue; scope:int}

and attributeValue =
| Int of int
| Bool of bool
| String of string
| Ref of string 
| Null 
and tableDescriptorClass= string*descriptorClass  

and data = {dcs: descriptorClass list; tm : tableMethod ; dos: descriptorObject list  }

(** PRINTING FUNCTIONS **)

let printValue attr = match attr with 
| Int(i) -> print_string (string_of_int i)
| Bool(b) -> print_string (string_of_bool b)
| String(str) -> print_string str
| Null -> print_string "null" 

let rec printClassAttributes astAttributes = match astAttributes with 
| a::t -> print_string a.aname; printClassAttributes t
| [] -> print_string "	End Class Attributes\n"

let rec print_list = function 
[] -> print_string "\n"; ()
| e::l -> print_string e ; print_string " " ; print_list l

let rec printDescriptorClass descriptorClass = match descriptorClass with 
| {classType= aType; methods= methods ;attributes= astattributes }::t -> 
	print_string ("class :"^stringOf(aType)^"\n"); print_string "	"; print_list methods; print_string "	End Methods \n"; printClassAttributes astattributes; print_string ("End class :"^stringOf(aType)^"\n"); printDescriptorClass t
| [] -> print_string "End Descriptors Class\n"

let rec printTableMethod tableMethod = match tableMethod with 
| a::t -> print_string "MethodFound : ";print_string (a.mname^"\n"); printTableMethod t
| [] -> print_string "End Table Methods\n"

let rec printDescriptorObject descriptorsObject = match descriptorsObject with 
| a::t -> print_string (a.objectName^" "); printValue a.objectValue;print_string " "; printDescriptorObject a.attributes ;printDescriptorObject t
| [] -> print_string "End Objects\n"

let printData data = match data with 
|{dcs= descriptorsClass ; tm = tableMethod ; dos= descriptorsObject  } -> printTableMethod tableMethod; printDescriptorClass descriptorsClass; printDescriptorObject descriptorsObject 
(****************************************************)

let classZone = []

let tableMethods = []

let buildData dcs tm dos = {dcs=dcs; tm=tm; dos=dos}

let buildRefType pack id = Type.Ref({
    tpath = pack ;
    tid = id ;
  })

let rec searchTypeList pack classType typeList = match typeList with 
	| {modifiers = modifiers; id = id; info = info}::t -> print_string ("searching :"^stringOf(classType)^"\n"); if classType = (buildRefType pack id) then {modifiers = modifiers; id = id; info = info}  else searchTypeList pack classType t
	| [] -> print_string "ERROR Class not found\n"; exit 1

let rec filterAttributes attributes matchedAttributes = match attributes with 
|{
      amodifiers = modifiers;
      aname = name;
      atype = atype;
      adefault = expression;
      (*      aloc : Location.t;*)
    } ::t -> if List.mem AST.Private modifiers then filterAttributes t matchedAttributes else filterAttributes t ({
      amodifiers = modifiers;
      aname = name;
      atype = atype;
      adefault = expression;
      (*      aloc : Location.t;*)
    } ::matchedAttributes)
|[] -> matchedAttributes	
let rec searchForAttributes classType dcs = match dcs with 
	| {classType=aType; methods=methods; attributes=astattributes}::t -> print_string ("searching attributes :"^stringOf(aType)^"\n"); if classType = aType then filterAttributes astattributes [] else searchForAttributes classType t
	| [] -> print_string "ERROR Class not found\n"; exit 1

let findClass ast classType = match ast with 
| {package = Some pack;type_list = typeList} -> searchTypeList pack classType typeList
| {package = None ;type_list = typeList} -> searchTypeList [] classType typeList

let rec notCompiled classType dcs = match dcs with 
| {classType= aType; methods= methods ;attributes=astattributes }::t -> if classType = aType then false else notCompiled classType t
| [] -> true

let rec methodsForDescriptor tableMethod regexpId listMethodsClass = match tableMethod with
| {mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t -> 
	if Str.string_match regexpId mname 0 then mname::(methodsForDescriptor t regexpId listMethodsClass) else methodsForDescriptor t regexpId listMethodsClass
| [] -> listMethodsClass

let rec methodsParentsForDescriptor tableMethod regexpId listMethodsClass = match tableMethod with
| {mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t -> 
	print_string ("PASSING IN"^mname^"\n");
	if Str.string_match regexpId mname 0 then 
		begin
			if List.mem AST.Private modifiers then begin print_string ("not taking"^mname^"\n"); methodsParentsForDescriptor t regexpId listMethodsClass end
			else begin print_string ("ADDING"^mname^"\n"); mname::(methodsParentsForDescriptor t regexpId listMethodsClass)  end 
		end	
	else begin print_string ("NOT TAKING"^mname^"\n"); methodsParentsForDescriptor t regexpId listMethodsClass end
| [] -> listMethodsClass

let rec addMethods className methods tableMethods = match methods with 
| {mmodifiers = modifiers;mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t -> 
	print_string ("adding method "^mname^"\n");
	let newTableMethod = {mmodifiers = modifiers;mname = className^"$"^mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}:: tableMethods in addMethods className t newTableMethod
| [] -> tableMethods

let rec addAttributes  listParentAttributes astattributeList = match listParentAttributes with 
| {amodifiers = modifiers;aname =aName;atype =aType ;adefault=expression;(*      aloc : Location.t;*)}::t ->
	let newAstAttribute = {amodifiers=modifiers; aname=aName; atype=aType; adefault=expression; (*      aloc : Location.t;*)}::astattributeList in 
	addAttributes t newAstAttribute
| [] -> astattributeList

let rec compile pack astTyped info id data= 
let classType = (buildRefType pack id) in
match info with 
| {cparent=ref_type; cattributes=astattributeList; cinits=initialList; cconsts=astconstList; cmethods=astmethodList; cloc=location;} when ref_type=object_type ->
 	if notCompiled classType data.dcs then
 		begin
		let newTableMethod = addMethods id astmethodList data.tm in
		let methods= methodsForDescriptor newTableMethod (Str.regexp_string (id^"$")) [] in
		let totalMethods = methodsParentsForDescriptor newTableMethod (Str.regexp_string (ref_type.tid^"$")) methods in
		let newAttributeListClass = addAttributes (searchForAttributes (Type.Ref(ref_type)) data.dcs) astattributeList in
		let descriptorClass = {classType=classType; methods=totalMethods; attributes = newAttributeListClass} in 
		buildData (descriptorClass::data.dcs) newTableMethod data.dos
		end
	else
		data
	
| {cparent = ref_type; cattributes = astattributeList;cinits = initialList; cconsts = astconstList; cmethods = astmethodList; cloc = location;} when ref_type!=object_type ->
	let parentClassType = Type.Ref(ref_type) in
	if notCompiled parentClassType data.dcs then 
		begin
		let newData = compile pack astTyped (findClass astTyped parentClassType).info ref_type.tid data in 
		let newTableMethod = addMethods id astmethodList newData.tm in
		let methods= methodsForDescriptor newTableMethod (Str.regexp_string (id^"$")) [] in
		print_list methods;print_string "\n\n";
		let totalMethods = methodsParentsForDescriptor newTableMethod (Str.regexp_string (ref_type.tid^"$")) methods in
		print_list totalMethods;print_string "\n\n";
		let newAttributeListClass = addAttributes (searchForAttributes (Type.Ref(ref_type)) newData.dcs) astattributeList in
		let descriptorClass = {classType=classType; methods=totalMethods; attributes = newAttributeListClass} in
		buildData (descriptorClass::newData.dcs) newTableMethod newData.dos
		end	
	else
		begin
		let newTableMethod = addMethods id astmethodList data.tm in
		let methods= methodsForDescriptor newTableMethod (Str.regexp_string (id^"$")) [] in
		let totalMethods = methodsParentsForDescriptor newTableMethod (Str.regexp_string (ref_type.tid^"$")) methods in
		let descriptorClass = {classType=classType; methods=totalMethods; attributes = addAttributes (searchForAttributes (Type.Ref(ref_type)) data.dcs) astattributeList} in
		buildData (descriptorClass::data.dcs) newTableMethod data.dos
		end
let rec typeListWalk pack astTyped type_list data = match type_list with 
| {modifiers = modifiers; id = id; info = info} :: t -> typeListWalk pack astTyped t (compile pack astTyped info id data)
| [] -> data


(*Class Object from java.lang package*)

let objectClass = {classType=Type.Ref{tpath=[]; tid="Object"}; methods=["equals"];attributes= []}

let object_equals obj1 obj2 = match obj1 with
| obj1 when obj1==obj2 -> true
| obj1 when obj1!=obj2 -> false

let integerClass = {classType=Primitive(Int); methods=[];attributes= []}

let initData =  {dcs= objectClass::[integerClass]; tm = [] ; dos= []  }

let treeWalk astTyped = match astTyped with
| {package = Some pack;type_list = typeList} -> typeListWalk pack astTyped typeList initData
| {package = None;type_list = typeList} -> typeListWalk [] astTyped typeList initData




