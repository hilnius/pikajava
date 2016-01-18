open AST

let rec map2 f l parameter = match l with
  | [] -> []
  | h::t -> (f h parameter) :: (map2 f t parameter)
;;

let typeValue v = match v with
  | Int(string) -> Type.Primitive(Int)
  | Float(string) -> Type.Primitive(Float)
  | Char(char) -> Type.Primitive(Char)
  | Boolean(bool) -> Type.Primitive(Boolean)
  (*| String(string) ->
  | Null *)
;;

let typeExpression e registry = match e.edesc with
  (* | New of string list * expression list
  | Call of expression * string * expression list
  | Attr of expression * string
  | If of expression * expression * expression *)
  | Val(value) -> e.etype <- Some(typeValue value); e
  (* | Name of string
  | ArrayInit of expression list
  | AssignExp of expression * assign_op * expression
  | Post of expression * postfix_op
  | Op of expression * infix_op * expression
  | CondOp of expression * expression * expression
  | Cast of expression * expression
  | Instanceof of expression * expression *)
;;

let typeVarDecl v registry = match v with
  | (t, varName, None) -> (t, varName, None)
  | (t, varName, Some(init)) -> (t, varName, Some(typeExpression init registry))
;;

let typeStatement s registry = match s with
  | VarDecl(l) -> VarDecl(map2 typeVarDecl l registry)
  (*| Block of statement list
  | Nop
  | While of expression * statement
  | For of (Type.t * string * expression option) list * expression option * expression list * statement
  | If of expression * statement * statement option
  | Return of expression option
  | Throw of expression
  | Try of statement list * (argument * statement list) list * statement list
  | Expr of expression *)
;;

let typeMethod meth registry = match meth with
  | { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = f } ->
    { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = map2 typeStatement f registry }
;;

let typeClass cl registry = match cl with
  | { modifiers = a; id = b; info = { cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = g; cloc = h } }->
    { modifiers = a; id = b; info = { cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = (map2 typeMethod g registry); cloc = h } }
;;

let typeAST ast registry =
  let { package = p; type_list = classList; } = ast in
    { package = p; type_list = (map2 typeClass classList registry); }
;;
