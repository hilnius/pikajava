open AST

(* Something to help with matching*)
type test = {name:string}

let test lol str = match lol with
|{name=var} when var = str  -> print_string str;
|_ -> print_string "egzegzegze"
(*------------------------------------------------*)
type descriptorObject = string

type methodType = {name:string; code: astmethod}

type tableMethod = methodType list

and descriptorClass = {name: string; methods: string list; attributes: string list}

and tableDescriptorClass= string*descriptorClass;;   

let classZone = []

let tableMethods = []

let buildDescriptor tableMethod regexpId = match tableMethod with 
| {mmodifiers = modifiers;mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t when Str.string_match regexpId mname 0-> print_string ("Method found"^mname^"\n")
| _ -> print_string "Method not found\n"

let rec addMethods className methods tableMethods = match methods with 
| {mmodifiers = modifiers;mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t -> print_string "adding method\n"; let newTableMethod = {mmodifiers = modifiers;mname = className^"_"^mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}:: tableMethods in addMethods className t newTableMethod
| [] -> tableMethods

let rec typeListWalk type_list = match type_list with 
| {modifiers = modifiers; id = id; info = {cparent = ref_type; cattributes = astattributeList;cinits = initialList; cconsts = astconstList; cmethods = astmethodList; cloc = location;}} :: t -> print_string "AAAA\n"; let newTableMethod = addMethods id astmethodList tableMethods in buildDescriptor newTableMethod (Str.regexp_string id) ; print_string "Finished\n" 
| [] -> print_string "BBBBB\n"

let treeWalk astTyped = match astTyped with
| {package = pack;type_list = typeList} -> typeListWalk typeList

(*Class Object from java.lang package*)

let objectClass = {name="Object"; methods=["equals"];attributes= []}

let object_equals obj1 obj2 = match obj1 with
| obj1 when obj1==obj2 -> true
| obj1 when obj1!=obj2 -> false
