open Compilation

let getMain tableMethod = match tableMethod with 
|{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}::t->
if (( List.mem AST.Public modifiers) && (List.mem AST.Static modifiers) && (List.length modifiers = 2) ) && (mname = "main") && (mreturntype = Type.Void) (* TEST ARGUMENT *) then
	{mmodifiers = modifiers; mname = mname;mreturntype = mreturntype;margstype = arguments;mthrows = exceptions;mbody = statements; (*      mloc : Location.t;*)}
else getMain t
|[] -> exit 0

 	
