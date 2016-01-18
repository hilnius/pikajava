open AST
open Exceptions
open ClassRegistry

let rec map2 f l parameter = match l with
  | [] -> []
  | h::t -> (f h parameter) :: (map2 f t parameter)
;;

let scope = ref [];;

let pushScope varType varName =
  scope := (varType, varName) :: (!scope)
;;

let popScope () =
  scope := List.tl (!scope)
;;

let getScopeType varName =
  let rec innerGetScope l = match l with
    | [] -> raise (VariableDoesNotExist(varName))
    | (vt, vn)::q when vn = varName -> vt
    | t::q -> innerGetScope q
  in
  innerGetScope (!scope);;

(*let rec getPathType l r = match l with
  | [] -> raise EmptyList
  | [a] -> getScopeType a
  | h::t -> let t1 = getPathType t r in
    match t1 with
      | AttributeSignature(t2) -> getClassType (Type.stringOf t2) h r
      | MethodSignature(t2, _) -> getClassType (Type.stringOf t2) h r
;;*)

let getMethodType e methodName arguments r = match e.etype with
  | Some(Type.Ref(refType)) -> getClassMethod refType.tid methodName arguments r
;;

let getAttributeType e name r = match e.etype with
  | Some(Type.Ref(refType)) -> getClassAttribute refType.tid name r
;;

let typeValue v = match v with
  | Int(string) -> Type.Primitive(Int)
  | Float(string) -> Type.Primitive(Float)
  | Char(char) -> Type.Primitive(Char)
  | Boolean(bool) -> Type.Primitive(Boolean)
  (*| String(string) ->
  | Null *)
;;

let rec typeExpression e r = match e.edesc with
  | New(identifiers, arguments) -> { etype = Some(Ref(Type.refOfStringList identifiers)); edesc = New(identifiers, map2 typeExpression arguments r) }
  | Call(e2, methodName, arguments) -> { etype = Some(getMethodType (typeExpression e2 r) methodName (map2 typeExpression arguments r) r); edesc = e.edesc }
  | Attr(e2, str) -> e.etype <- Some(getAttributeType (typeExpression e2 r) str r); e
  | If(e, ifSt, elseSt) -> e.etype <- Some(Void); e
  | Val(value) -> e.etype <- Some(typeValue value); e
  | Name(varName) -> e.etype <- Some(getScopeType varName); e
  (*| ArrayInit of expression list
  | AssignExp of expression * assign_op * expression
  | Post of expression * postfix_op *)
  | Op(e1, op, e2) -> begin match op with
      | Op_eq -> { etype = Some(Primitive(Boolean)); edesc = Op(typeExpression e1 r, op, typeExpression e2 r) }
    end
  (*| CondOp of expression * expression * expression
  | Cast of expression * expression
  | Instanceof of expression * expression *)
;;

let typeVarDecl v r = match v with
  | (t, varName, None) -> pushScope t varName; (t, varName, None)
  | (t, varName, Some(init)) -> pushScope t varName; (t, varName, Some(typeExpression init r))
;;

let rec typeCatches c r = match c with
  | [] -> []
  | (a,b)::q -> (a, map2 typeStatement b r) :: typeCatches q r

and typeStatement s r = match s with
  | VarDecl(l) -> VarDecl(map2 typeVarDecl l r)
  | Block(sl) -> Block(map2 typeStatement sl r)
  | Nop -> Nop
  | While(e, s) -> While(typeExpression e r, typeStatement s r)
  | For(vdl, None, el, st) -> For(map2 typeVarDecl vdl r, None, map2 typeExpression el r, typeStatement st r)
  | For(vdl, Some(ec), el, st) -> For(map2 typeVarDecl vdl r, Some(typeExpression ec r), map2 typeExpression el r, typeStatement st r)
  | If(test, ifSt, None) -> If(typeExpression test r, typeStatement ifSt r, None)
  | If(test, ifSt, Some(elseSt)) -> If(typeExpression test r, typeStatement ifSt r, Some(typeStatement elseSt r))
  | Return(None) -> Return(None)
  | Return(Some(e)) -> Return(Some(typeExpression e r))
  | Throw(e) -> Throw(typeExpression e r)
  | Try(sl, catches, finally) -> Try(map2 typeStatement sl r, typeCatches catches r, map2 typeStatement finally r)
  | Expr(e) -> Expr(typeExpression e r)
;;

let typeMethod meth r = match meth with
  | { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = f } ->
    { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = map2 typeStatement f r }
;;

let typeClass cl r = match cl with
  | { modifiers = a; id = b; info = { cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = g; cloc = h } }->
    pushScope (Type.Ref({ tpath = []; tid = b})) "this";
    { modifiers = a; id = b; info = { cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = (map2 typeMethod g r); cloc = h } }
;;

let typeAST ast registry =
  let { package = p; type_list = classList; } = ast in
    { package = p; type_list = (map2 typeClass classList registry); }
;;
