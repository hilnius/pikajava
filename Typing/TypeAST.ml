open AST
open Exceptions
open ClassRegistry

let rec map2 f l parameter = match l with
  | [] -> []
  | h::t -> (f h parameter) :: (map2 f t parameter)
;;

type scopeVariable = Type.t * string


let scope : scopeVariable list list ref = ref [];;

let enterScope () =
  print_string (generateTabs (List.length !scope) ^ "{\n");
  scope := []::(!scope)
;;

let exitScope () =
  print_string (generateTabs ((List.length !scope) - 1)  ^ "}\n");
  match !scope with
  | [] -> raise ScopeDoesNotExist
  | h::t -> scope := t
;;

(* checks if a variable is already declared in the scope, in which case we should raise an exception *)
let currentScopeVariableExists varName =
  let rec innerScopeVariableExists l = match l with
    | [] -> false
    | (_, n)::q when n = varName -> true
    | _::q -> innerScopeVariableExists q
  in
  match !scope with
    | [] -> raise ScopeDoesNotExist
    | t::_ -> innerScopeVariableExists t
;;

let declareVariable varType varName =
  print_string (generateTabs (List.length !scope) ^ "Declaring variable " ^ varName ^ " of type " ^ Type.stringOf varType ^ "\n");
  if currentScopeVariableExists varName then
    raise (VariableAlreadyDeclared varName);
  match !scope with
  | [] -> raise ScopeDoesNotExist
  | h::t -> scope := ((varType, varName)::h)::t
;;

let getScopeType varName =
  let rec innerGetScope l = match l with
    | [] -> None
    | (vt, vn)::q when vn = varName -> Some(vt)
    | _::t -> innerGetScope t
  in
  let rec goDownScopes l = match l with
    | [] -> raise (VariableDoesNotExist(varName))
    | h::t -> let a = innerGetScope h in
      match a with
        | None -> goDownScopes t
        | Some(t) -> t
  in
  goDownScopes (!scope);;

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
  | Int(_) -> Type.Primitive(Int)
  | Float(_) -> Type.Primitive(Float)
  | Char(_) -> Type.Primitive(Char)
  | Boolean(_) -> Type.Primitive(Boolean)
  | String(_) -> Type.Ref({ tpath = []; tid = "String" })
  | Null -> Type.NullReference
;;

let getConvertibleType t1 t2 = match t1, t2 with
  | (Type.Primitive(Int), Type.Primitive(Int)) -> Type.Primitive(Int)
  | (Type.Primitive(Char), Type.Primitive(Int)) -> Type.Primitive(Int)
  | (Type.Primitive(Int), Type.Primitive(Char)) -> Type.Primitive(Int)
  | (Type.Primitive(Char), Type.Primitive(Char)) -> Type.Primitive(Char)
  | (Type.Primitive(Boolean), Type.Primitive(Boolean)) -> Type.Primitive(Boolean)
  | _, _ -> raise (CannotConvertTypes (t1, t2))
;;

let getConvertibleNumericType t1 t2 = match t1, t2 with
  | (Type.Primitive(Int),   Type.Primitive(Int)) -> Type.Primitive(Int)
  | (Type.Primitive(Char),  Type.Primitive(Int)) -> Type.Primitive(Int)
  | (Type.Primitive(Int),   Type.Primitive(Char)) -> Type.Primitive(Int)
  | (Type.Primitive(Char),  Type.Primitive(Char)) -> Type.Primitive(Char)
  | (Type.Primitive(Float), Type.Primitive(Float)) -> Type.Primitive(Float)
  | (Type.Primitive(Int),   Type.Primitive(Float)) -> Type.Primitive(Float)
  | (Type.Primitive(Float), Type.Primitive(Int)) -> Type.Primitive(Float)
  | (Type.Primitive(Char),  Type.Primitive(Float)) -> Type.Primitive(Float)
  | (Type.Primitive(Float), Type.Primitive(Char)) -> Type.Primitive(Float)
  | _, _ -> raise (CannotConvertTypes (t1, t2))
;;

let getConvertibleNumericOrStringType t1 t2 = match t1, t2 with
  | (Type.Ref({ tpath = []; tid = "String" }), Type.Ref({ tpath = []; tid = "String" })) -> Type.Ref({ tpath = []; tid = "String" })
  | _, _ -> getConvertibleNumericType t1 t2
;;

let rec typeExpression e r = match e.edesc with
  | New(name, identifiers, arguments) -> { etype = Some(Ref(Type.refOfStringList identifiers)); edesc = New(name, identifiers, map2 typeExpression arguments r) }
  | NewArray(t1, eol, eo) -> { etype = Some(Type.mk_array (List.length eol) t1); edesc = NewArray(t1, map2 typeExpressionOption eol r, typeExpressionOption eo r) }
  | Call(e2, methodName, arguments) -> begin
    match e2 with
      | None -> { etype = None; edesc = e.edesc }
      | Some(e2s) -> { etype = Some(getMethodType (typeExpression e2s r) methodName (map2 typeExpression arguments r) r); edesc = e.edesc }
    end
  | Attr(e2, str) -> e.etype <- Some(getAttributeType (typeExpression e2 r) str r); e
  | If(e1, ifSt, elseSt) -> { etype = None; edesc = If(typeExpression e1 r, typeExpression ifSt r, typeExpression elseSt r) } (* wtf is this If ? *)
  | Val(value) -> e.etype <- Some(typeValue value); e
  | Name(varName) -> e.etype <- Some(getScopeType varName); e
  | ArrayInit(el) -> { etype = None; edesc = ArrayInit(map2 typeExpression el r) }
  | Array(e1, eol)-> let t1 = typeExpression e1 r in { etype = Some(Type.mk_array (List.length eol) (CheckAST.extractSome (t1.etype))); edesc = Array(t1, map2 typeExpressionOption eol r) }
  | AssignExp(e1, aop, e2) -> let te1 = typeExpression e1 r in { etype = te1.etype; edesc = AssignExp(te1, aop, typeExpression e2 r) }
  | Post(e1, pfo) ->  let t = typeExpression e1 r in { etype = t.etype; edesc = Post(t, pfo) }
  | Pre(pfo, e1) -> let t = typeExpression e1 r in { etype = t.etype; edesc = Pre(pfo, t) }
  | Op(e1, op, e2) -> let (t1,t2) = typeExpression e1 r, typeExpression e2 r in begin
    match op with
      | Op_cor -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_cand -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_or -> let ct = getConvertibleType (CheckAST.extractSome t1.etype) (CheckAST.extractSome t2.etype) in
                  { etype = Some(ct); edesc = Op(t1, op, t2) }
      | Op_and -> let ct = getConvertibleType (CheckAST.extractSome t1.etype) (CheckAST.extractSome t2.etype) in
                  { etype = Some(ct); edesc = Op(t1, op, t2) }
      | Op_xor -> let ct = getConvertibleType (CheckAST.extractSome t1.etype) (CheckAST.extractSome t2.etype) in
                  { etype = Some(ct); edesc = Op(t1, op, t2) }
      | Op_eq -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_ne -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_gt -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_lt -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_ge -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_le -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_shl -> {  etype = Some(Primitive(Int)); edesc = Op(t1, op, t2) } (* those three shl shr shrr must convert to integral types *)
      | Op_shr -> {  etype = Some(Primitive(Int)); edesc = Op(t1, op, t2) }
      | Op_shrr -> { etype = Some(Primitive(Int)); edesc = Op(t1, op, t2) }
      | Op_add -> let ct = getConvertibleNumericOrStringType (CheckAST.extractSome t1.etype) (CheckAST.extractSome t2.etype) in
                    { etype = Some(ct); edesc = Op(t1, op, t2) }
      | Op_sub -> let ct = getConvertibleNumericType (CheckAST.extractSome t1.etype) (CheckAST.extractSome t2.etype) in
                    { etype = Some(ct); edesc = Op(t1, op, t2) }
      | Op_mul -> let ct = getConvertibleNumericType (CheckAST.extractSome t1.etype) (CheckAST.extractSome t2.etype) in
                    { etype = Some(ct); edesc = Op(t1, op, t2) }
      | Op_div -> let ct = getConvertibleNumericType (CheckAST.extractSome t1.etype) (CheckAST.extractSome t2.etype) in
                    { etype = Some(ct); edesc = Op(t1, op, t2) }
      | Op_mod -> let ct = getConvertibleNumericType (CheckAST.extractSome t1.etype) (CheckAST.extractSome t2.etype) in
                    { etype = Some(ct); edesc = Op(t1, op, t2) }
    end
  | CondOp(e1,e2,e3) -> let t2 = typeExpression e2 r in { etype = t2.etype; edesc = CondOp (typeExpression e1 r, t2, typeExpression e3 r) }
  | Cast(t1, e1) -> { etype = Some(t1); edesc = Cast(t1, typeExpression e1 r) }
  | Instanceof(e1, t1) -> { etype = Some(Primitive(Boolean)); edesc = Instanceof(typeExpression e1 r, t1) }
  | ClassOf(t1) -> e.etype <- Some(t1); e
  | VoidClass -> e

and typeExpressionOption e r = match e with
  | None -> None
  | Some(e1) -> Some(typeExpression e1 r)


and typeVarDecl v r = match v with
  | (t, varName, None) -> declareVariable t varName; (t, varName, None)
  | (t, varName, Some(init)) -> declareVariable t varName; (t, varName, Some(typeExpression init r))


and typeVarDeclOpt v r = match v with
  | (Some(t), varName, None) -> declareVariable t varName; (Some(t), varName, None)
  | (Some(t), varName, Some(init)) -> declareVariable t varName; (Some(t), varName, Some(typeExpression init r))
  | (None, varName, None) -> (None, varName, None)
  | (None, varName, Some(init)) -> (None, varName, Some(typeExpression init r))


and typeCatches c r = match c with
  | [] -> []
  | (a,b)::q ->
      enterScope ();
      let b2 = List.rev (map2 typeStatement (List.rev b) r) in
      exitScope();
      (a, b2) :: typeCatches q r

and typeStatement s r = match s with
  | VarDecl(l) -> VarDecl(map2 typeVarDecl l r)
  | Block(sl) ->
      enterScope ();
      let b = Block(List.rev (map2 typeStatement (List.rev sl) r)) in
      exitScope();
      b
  | Nop -> Nop
  | While(e, s) ->
      enterScope ();
      let ws = typeStatement s r in
      exitScope();
      While(typeExpression e r, ws)
  | For(vdl, None, el, st) ->
      let m1 = List.rev (map2 typeVarDeclOpt (List.rev vdl) r) in
      enterScope ();
      let st2 = typeStatement st r in
      exitScope ();
      For(m1, None, List.rev (map2 typeExpression (List.rev el) r), st2)
  | For(vdl, Some(ec), el, st) ->
      let vdl2 = List.rev (map2 typeVarDeclOpt (List.rev vdl) r) in
      enterScope ();
      let st2 = typeStatement st r in
      exitScope ();
      For(vdl2, Some(typeExpression ec r), List.rev (map2 typeExpression (List.rev el) r), st2)
  | If(test, ifSt, None) ->
      enterScope ();
      let b = typeStatement ifSt r in
      exitScope();
      If(typeExpression test r, b, None)
  | If(test, ifSt, Some(elseSt)) ->
      enterScope ();
      let a = typeStatement ifSt r in
      exitScope();
      enterScope ();
      let b = typeStatement elseSt r in
      exitScope();
      If(typeExpression test r, a, Some(b))
  | Return(None) -> Return(None)
  | Return(Some(e)) -> Return(Some(typeExpression e r))
  | Throw(e) -> Throw(typeExpression e r)
  | Try(sl, catches, finally) ->
      enterScope ();
      let sl2 = List.rev (map2 typeStatement (List.rev sl) r) in
      exitScope();
      let catches2 = typeCatches catches r in
      enterScope ();
      let finally2 = List.rev (map2 typeStatement (List.rev finally) r) in
      exitScope();
      Try(sl2, catches2, finally2)
  | Expr(e) -> Expr(typeExpression e r)
;;

let typeMethod meth r = match meth with
  | { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = f } ->
    { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = List.rev (map2 typeStatement (List.rev f) r) }
;;

let typeClass cl r = match cl with
  | { modifiers = a; id = b; info = Class({ cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = g; ctypes = t; cloc = h }) }->
    enterScope ();
    declareVariable (Type.Ref({ tpath = []; tid = b})) "this";
    let a = { modifiers = a; id = b; info = Class({ cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = (map2 typeMethod g r); ctypes = t; cloc = h }) }
    in
    exitScope();
    a
;;

let typeAST ast registry =
  let { package = p; type_list = classList; } = ast in
    { package = p; type_list = (map2 typeClass classList registry); }
;;
