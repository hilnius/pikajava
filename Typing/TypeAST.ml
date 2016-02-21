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

let declareVariable varType varName =
  print_string (generateTabs (List.length !scope) ^ "Declaring variable " ^ varName ^ "\n");
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
  | Int(string) -> Type.Primitive(Int)
  | Float(string) -> Type.Primitive(Float)
  | Char(char) -> Type.Primitive(Char)
  | Boolean(bool) -> Type.Primitive(Boolean)
  (*| String(string) ->
  | Null *)
;;

let rec typeExpression e r = match e.edesc with
  | New(name, identifiers, arguments) -> { etype = Some(Ref(Type.refOfStringList identifiers)); edesc = New(name, identifiers, map2 typeExpression arguments r) }
  | Call(e2, methodName, arguments) -> begin
    match e2 with
      | None -> { etype = None; edesc = e.edesc }
      | Some(e2s) -> { etype = Some(getMethodType (typeExpression e2s r) methodName (map2 typeExpression arguments r) r); edesc = e.edesc }
    end
  | Attr(e2, str) -> e.etype <- Some(getAttributeType (typeExpression e2 r) str r); e
  | If(e, ifSt, elseSt) -> e.etype <- Some(Void); e
  | Val(value) -> e.etype <- Some(typeValue value); e
  | Name(varName) -> e.etype <- Some(getScopeType varName); e
  (*| ArrayInit(el) -> print_string "typing array init\n"; { etype = None; edesc = ArrayInit(map2 typeExpression el r) }*)
  | AssignExp(e1, aop, e2) -> let te1 = typeExpression e1 r in { etype = te1.etype; edesc = AssignExp(te1, aop, typeExpression e2 r) }
  | Post(e1, pfo) ->  begin match pfo with (* not working yet *)
      | Incr -> let t = typeExpression e1 r in { etype = t.etype; edesc = Post(t, pfo) }
      | Decr -> let t = typeExpression e1 r in { etype = t.etype; edesc = Post(t, pfo) }
    end
  | Op(e1, op, e2) -> let (t1,t2) = typeExpression e1 r, typeExpression e2 r in begin
    match op with
      | Op_cor -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_cand -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      (*| Op_or -> { etype = Some(); edesc = Op(t1, op, t2) }
      | Op_and -> { etype = Some(); edesc = Op(t1, op, t2) }
      | Op_xor -> { etype = Some(); edesc = Op(t1, op, t2) }*)
      | Op_eq -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_ne -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_gt -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_lt -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_ge -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      | Op_le -> { etype = Some(Primitive(Boolean)); edesc = Op(t1, op, t2) }
      (*| Op_shl -> { etype = Some(); edesc = Op(t1, op, t2) }
      | Op_shr -> { etype = Some(); edesc = Op(t1, op, t2) }
      | Op_shrr -> { etype = Some(); edesc = Op(t1, op, t2) }*)
      (*| Op_add -> { etype = Some(); edesc = Op(t1, op, t2) }
      | Op_sub -> { etype = Some(); edesc = Op(t1, op, t2) }
      | Op_mul -> { etype = Some(); edesc = Op(t1, op, t2) }
      | Op_div -> { etype = Some(); edesc = Op(t1, op, t2) }
      | Op_mod -> { etype = Some(); edesc = Op(t1, op, t2) }*)
    end
  | CondOp(e1,e2,e3) -> let t2 = typeExpression e2 r in { etype = t2.etype; edesc = CondOp (typeExpression e1 r, t2, typeExpression e3 r) }
  | Cast(t1, e1) -> { etype = Some(t1); edesc = Cast(t1, typeExpression e1 r) }
  | Instanceof(e1, t1) -> { etype = Some(Primitive(Boolean)); edesc = Instanceof(typeExpression e1 r, t1) }
;;

let typeVarDecl v r = match v with
  | (t, varName, None) -> declareVariable t varName; (t, varName, None)
  | (t, varName, Some(init)) -> declareVariable t varName; (t, varName, Some(typeExpression init r))
;;

let typeVarDeclOpt v r = match v with
  | (Some(t), varName, None) -> declareVariable t varName; (Some(t), varName, None)
  | (Some(t), varName, Some(init)) -> declareVariable t varName; (Some(t), varName, Some(typeExpression init r))
  | (None, varName, None) -> (None, varName, None)
  | (None, varName, Some(init)) -> (None, varName, Some(typeExpression init r))
;;

let rec typeCatches c r = match c with
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
      enterScope ();
      let m1 = List.rev (map2 typeVarDeclOpt (List.rev vdl) r) in
      let st2 = typeStatement st r in
      exitScope ();
      For(m1, None, List.rev (map2 typeExpression (List.rev el) r), st2)
  | For(vdl, Some(ec), el, st) ->
      enterScope ();
      let vdl2 = List.rev (map2 typeVarDeclOpt (List.rev vdl) r) in
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
