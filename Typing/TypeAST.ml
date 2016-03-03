open AST
open Exceptions
open ClassRegistry

(* map function when f expects another parameter than the list elements *)
let rec map2 f l parameter = match l with
  | [] -> []
  | h::t -> (f h parameter) :: (map2 f t parameter)
;;

(* a scope variable, its type and its name *)
type scopeVariable = Type.t * string

(* scopes are nested lists of scope variables. Each scope is a list containing a list of scopeVariables *)
let scope : scopeVariable list list ref = ref [];;

(* cann this when entering a scope (while typing), to create a new scope in the stack *)
let enterScope () =
  scope := []::(!scope)
;;

(* exist the current scope (remove it from the stack). Variables defined in the scope won't be accessible anymore *)
let exitScope () =
  match !scope with
  | [] -> raise ScopeDoesNotExist
  | h::t -> scope := t
;;

(* checks if a variable is already declared in the scope, returns true if it exists, false otherwise *)
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

(* declare a variable in the current scope, specify the type and name *)
let declareVariable varType varName =
  if currentScopeVariableExists varName then (* first check that is doesn't exist yet *)
    raise (VariableAlreadyDeclared varName);
  match !scope with
  | [] -> raise ScopeDoesNotExist
  | h::t -> scope := ((varType, varName)::h)::t
;;

(* get the return type of a method depending on the class name, the method name, and the arguments *)
let getMethodType etype methodName arguments r = match etype with
  | Some(Type.Ref(refType)) -> getClassMethod refType.tid methodName arguments r
;;

(* get the type of an attribute depending on the class name and the attribute name *)
let getAttributeType etype name r = match etype with
  | Some(Type.Ref(refType)) -> getClassAttribute refType.tid name r
;;

(* get the type of a scope variable, if it exists *)
let getScopeType varName r =
  let rec innerGetScope l name = match l with (* find in a scope *)
    | [] -> None
    | (vt, vn)::q when vn = name -> Some(vt)
    | _::t -> innerGetScope t name
  in
  let rec goDownScopes l name = match l with (* check the topmost scope first, then go down *)
    | [] -> raise (VariableDoesNotExist(name))
    | h::t ->
      (match innerGetScope h name with
        | None -> begin
            (* if we're looking for 'hello' variable, check if 'this.hello' is defined before going down scopes *)
            match innerGetScope h "this" with
              | None -> goDownScopes t name
              | Some(t1) -> getAttributeType (Some t1) name r
          end;
        | Some(t2) -> t2)
  in
  goDownScopes (!scope) varName
;;

(* type a value expression *)
let typeValue v = match v with
  | Int(_) -> Type.Primitive(Int)
  | Float(_) -> Type.Primitive(Float)
  | Char(_) -> Type.Primitive(Char)
  | Boolean(_) -> Type.Primitive(Boolean)
  | String(_) -> Type.Ref({ tpath = []; tid = "String" })
  | Null -> Type.NullReference
;;

(* when trying to use integral types operations, check if types are compatible, and return the return type
  if types are not compatible, raise an exception *)
let getConvertibleType t1 t2 = match t1, t2 with
  | (Type.Primitive(Int), Type.Primitive(Int)) -> Type.Primitive(Int)
  | (Type.Primitive(Char), Type.Primitive(Int)) -> Type.Primitive(Int)
  | (Type.Primitive(Int), Type.Primitive(Char)) -> Type.Primitive(Int)
  | (Type.Primitive(Char), Type.Primitive(Char)) -> Type.Primitive(Char)
  | (Type.Primitive(Boolean), Type.Primitive(Boolean)) -> Type.Primitive(Boolean)
  | _, _ -> raise (CannotConvertTypes (t1, t2))
;;

(* when doing numeric operations, check if types are compatible and give the return type
  raises an exception if the types are not compatible *)
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

(* just for additions, check if types are both strings, or if they are numeric and are compatible *)
let getConvertibleNumericOrStringType t1 t2 = match t1, t2 with
  | (Type.Ref({ tpath = []; tid = "String" }), Type.Ref({ tpath = []; tid = "String" })) -> Type.Ref({ tpath = []; tid = "String" })
  | _, _ -> getConvertibleNumericType t1 t2
;;

(* recursive function to type expressions *)
let rec typeExpression e r = match e.edesc with
  | New(name, identifiers, arguments) -> { etype = Some(Ref(Type.refOfStringList identifiers)); edesc = New(name, identifiers, map2 typeExpression arguments r) }
  | NewArray(t1, eol, eo) -> { etype = Some(Type.mk_array (List.length eol) t1); edesc = NewArray(t1, map2 typeExpressionOption eol r, typeExpressionOption eo r) }
  | Call(e2, methodName, arguments) -> begin
    match e2 with
      | None -> { etype = Some(getMethodType (typeExpression { etype = None; edesc = (Name "this") } r).etype methodName (map2 typeExpression arguments r) r); edesc = Call(e2, methodName, arguments) }
      | Some(e2s) -> { etype = Some(getMethodType (typeExpression e2s r).etype methodName (map2 typeExpression arguments r) r); edesc = Call(Some(typeExpression e2s r), methodName, arguments) }
    end
  | Attr(e2, str) -> e.etype <- Some(getAttributeType (typeExpression e2 r).etype str r); e
  | If(e1, ifSt, elseSt) -> { etype = None; edesc = If(typeExpression e1 r, typeExpression ifSt r, typeExpression elseSt r) } (* wtf is this If ? *)
  | Val(value) -> e.etype <- Some(typeValue value); e
  | Name(varName) -> e.etype <- Some(getScopeType varName r); e
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

(* type an optional expression *)
and typeExpressionOption e r = match e with
  | None -> None
  | Some(e1) -> Some(typeExpression e1 r)

(* type a variable declaration. this declares a variable in the current scope *)
and typeVarDecl v r = match v with
  | (t, varName, None) -> declareVariable t varName; (t, varName, None)
  | (t, varName, Some(init)) -> declareVariable t varName; (t, varName, Some(typeExpression init r))

(* type an optional variable declaration (for for loops). this declares a variable in the current scope *)
and typeVarDeclOpt v r = match v with
  | (Some(t), varName, None) -> declareVariable t varName; (Some(t), varName, None)
  | (Some(t), varName, Some(init)) -> declareVariable t varName; (Some(t), varName, Some(typeExpression init r))
  | (None, varName, None) -> (None, varName, None)
  | (None, varName, Some(init)) -> (None, varName, Some(typeExpression init r))

(* type multiple catch statements, creating a scope for each and typing its content *)
and typeCatches c r = match c with
  | [] -> []
  | (a,b)::q ->
      enterScope ();
      let b2 = List.rev (map2 typeStatement (List.rev b) r) in
      exitScope();
      (a, b2) :: typeCatches q r

(* type a statement *)
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

(* declare a method argument. This creates a local variable in the method scope *)
let declareArgument argument = match argument with
  | { final = _; vararg = _; ptype = t1; pident = id; } -> declareVariable t1 id; argument
;;

(* type a method. this creates a scope before typing the method body *)
let typeMethod meth r = match meth with
  | { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = f } ->
    enterScope ();
    List.map declareArgument d;
    let methodBody = List.rev (map2 typeStatement (List.rev f) r) in
    exitScope ();
    { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = methodBody }
;;

(* type an initial (static/non-static initialization block). this creates a scope before typing statements *)
let typeInitial i r = match i with
  | { static = b; block = statements } ->
      enterScope ();
      let typedInits = List.rev (map2 typeStatement (List.rev statements) r) in
      exitScope ();
      { static = b; block = typedInits }
;;

(* type a class content. this creates a scope in which "this" is defined, so that "this" calls refer to the current class *)
let typeClass cl r = match cl with
  | { modifiers = a; id = b; info = Class({ cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = g; ctypes = t; cloc = h }) } ->
    registerClassParent b c.tid;
    enterScope ();
    declareVariable (Type.Ref({ tpath = []; tid = b})) "this";
    let typedInits = List.rev (map2 typeInitial (List.rev e) r) in
    let typedMethods = map2 typeMethod g r in
    exitScope();
    { modifiers = a; id = b; info = Class({ cparent = c; cattributes = d; cinits = typedInits; cconsts = f; cmethods = typedMethods; ctypes = t; cloc = h }) }
;;

(* type an ast *)
let typeAST ast registry =
  let { package = p; type_list = classList; } = ast in
    { package = p; type_list = (map2 typeClass classList registry); }
;;
