open Type
open AST
open Exceptions
open ClassRegistry

(* function when f expects another parameter than the list elements *)
let iterArg f l parameter =
  let fp sd = f sd parameter in
    List.iter fp l
;;

let iterArg2 f l parameter parameter2 =
  let fp sd = f sd parameter parameter2 in
    List.iter fp l
;;

let extractSome a = match a with
  | None -> raise UntypedExpression;
  | Some(b) -> b;
;;

let comparePrimitiveTypes p1 p2 = match p1, p2 with
  | Type.Boolean, Type.Boolean -> true
  | Type.Boolean, _ -> false
  | _, Type.Boolean -> false
  | _ -> true
;;

let compareTypes t1 t2 = match t1, t2 with
  | Type.Primitive(p1), Type.Primitive(p2) -> comparePrimitiveTypes p1 p2
  | Type.Ref(r1), Type.Ref(r2) -> (checkClassInstanceOf r1.tid r2.tid || checkClassInstanceOf r2.tid r1.tid)
  | Type.NullReference, Type.Ref(_) -> true
  | Type.Ref(_), Type.NullReference -> true
  | _ -> t1 == t2
;;

let compareAssignTypes t1 t2 = match t1, t2 with
  | Type.Ref(r1), Type.Ref(r2) -> checkClassInstanceOf r2.tid r1.tid
  | _ -> compareTypes t1 t2
;;

let checkBoolean t1 = match t1 with
  | Type.Primitive(Type.Boolean) -> true
  | _ -> false
;;

let checkPrimitiveNotBoolean t1 = match t1 with
  | Type.Primitive(Type.Boolean) -> false
  | Type.Primitive(_) -> true
  | _ -> false
;;

let checkIntegerKind t1 = match t1 with
  | Type.Primitive(Type.Int) | Type.Primitive(Type.Long) | Type.Primitive(Type.Short) -> true
  | _ -> false
;;

let checkCall e methodName arguments reg = match e with
  | None -> ();
  | Some(exp) -> match extractSome exp.etype with
    | Type.Ref(r) -> ()
    | _ -> raise (NotDeferencable(extractSome exp.etype))

let checkExpression e reg = match e.edesc with
  | Call(e2, methodName, arguments) -> checkCall e2 methodName arguments reg
  (*| ArrayInit of expression list*)
  | AssignExp(e1, aop, e2) -> begin match aop with
      | Assign | Ass_add | Ass_sub | Ass_mul | Ass_div -> if not(compareAssignTypes (extractSome e1.etype) (extractSome e2.etype)) then raise (CannotCompareTypes(extractSome e1.etype, extractSome e2.etype))
      | Ass_mod -> if not(checkPrimitiveNotBoolean (extractSome e1.etype) && checkPrimitiveNotBoolean (extractSome e2.etype)) then raise (BadOperandTypes(extractSome e1.etype, extractSome e2.etype))
      | Ass_shl | Ass_shr | Ass_shrr -> if not(checkIntegerKind (extractSome e1.etype) && checkIntegerKind (extractSome e2.etype)) then raise (BadOperandTypes(extractSome e1.etype, extractSome e2.etype))
    end;
  | Post(e1, pfo) -> if not(checkIntegerKind (extractSome e1.etype)) then raise (CannotCast(extractSome e1.etype, Type.Primitive(Type.Int)))
  | Pre(pfo, e1) -> begin match pfo with
      | Op_incr | Op_decr -> if not(checkIntegerKind (extractSome e1.etype)) then raise (CannotCast(extractSome e1.etype, Type.Primitive(Type.Int)))
    end;
  | Op(e1, op, e2) -> begin match op with
      | Op_eq | Op_ne | Op_gt | Op_lt | Op_ge | Op_le -> if not(compareTypes (extractSome e1.etype) (extractSome e2.etype)) then raise (CannotCompareTypes(extractSome e1.etype, extractSome e2.etype))
      | Op_cor | Op_cand -> begin
         if not (checkBoolean (extractSome e1.etype)) then raise (ShouldBeBoolean(extractSome e1.etype));
         if not (checkBoolean (extractSome e2.etype)) then raise (ShouldBeBoolean(extractSome e2.etype))
      end;
    end;
  | CondOp(e1, e2, e3) -> begin
      if not (checkBoolean (extractSome e1.etype)) then raise (ShouldBeBoolean(extractSome e1.etype));
      if not (compareTypes (extractSome e2.etype) (extractSome e3.etype)) then raise (CannotCompareTypes(extractSome e2.etype, extractSome e3.etype))
    end;
  (*| Cast(t1, e1) -> print_string "hello"*)
  (*| Instanceof of expression * expression *)
  (*| Cast(t1, e1) -> print_string("Attr1")
  | Instanceof(e1, t1) -> print_string("Attr2")
  | VoidClass -> print_string("Attr3")
  | ClassOf(t1) -> print_string("Attr4")
  | New(name, identifiers, arguments) -> print_string("Attr5")
  | NewArray(t1, eol, eo) -> print_string("Attr6")
  | Call(e2, methodName, arguments) -> print_string("Attr7")
  | Attr(e2, str) ->print_string("Attr8")
  | If(e1, ifSt, elseSt) -> print_string("Attr9")
  | Val(value) -> print_string("Attr10")
  | Name(varName) ->print_string("Attr11")
  | ArrayInit(el) ->print_string("Attr12")
  | Array(e1, eol)->print_string("Attr13")
  | Post(e1, pfo) -> print_string("Attr")
  | Pre(pfo, e1) -> print_string("Attr")*)
  | _ -> ()
;;

let checkVarDecl v reg = match v with
  | (t, varName, None) -> ()
  | (t, varName, Some(init)) ->
    checkExpression init;
    let t2 = extractSome init.etype in
    if not (compareAssignTypes t t2) then begin
      raise (TypeMismatch(t, t2))
    end
;;

let checkVarDeclOpt v reg = match v with
  | (None, varName, init) -> ()
  | (Some(t), varName, init) -> checkVarDecl (t varName init) reg

let rec checkStatement s reg returnType = match s with
  | VarDecl(l) -> iterArg checkVarDecl l reg
  | Block(sl) -> iterArg2 checkStatement sl reg returnType
  | While(e, s) -> begin checkExpression e reg; checkStatement s reg returnType end
  | For(vdl, None, el, st) -> begin
      (*iterArg checkVarDeclOpt vdl reg;*)
      iterArg checkExpression el reg;
      checkStatement st reg returnType
    end
  | For(vdl, Some(ec), el, st) -> begin
      (*iterArg checkVarDeclOpt vdl reg;*)
      checkExpression ec reg;
      iterArg checkExpression el reg;
      checkStatement st reg returnType
    end
  | If(e1, ifSt, None) -> checkExpression e1 reg; if extractSome (e1.etype) <> Primitive(Boolean) then raise (ShouldBeBoolean(extractSome e1.etype)); checkStatement ifSt reg returnType;
  | If(e1, ifSt, Some(elseSt)) -> checkExpression e1 reg; if extractSome (e1.etype) <> Primitive(Boolean) then raise (ShouldBeBoolean(extractSome e1.etype)); checkStatement ifSt reg returnType; checkStatement elseSt reg returnType
  | Return(None) -> if not (compareTypes Type.Void returnType) then raise (CannotCompareTypes(Type.Void, returnType))
  | Return(Some(e)) -> if not (compareTypes (extractSome e.etype) returnType) then raise (CannotCompareTypes(extractSome e.etype, returnType))
  | Expr(e) -> checkExpression e reg
  | _ -> ()
;;

let checkMethod meth reg = match meth with
  | { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = f } -> iterArg2 checkStatement f reg c
;;

let checkClass cl reg = match cl with
  | { modifiers = a; id = b; info = Class({ cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = g; cloc = h }) } -> iterArg checkMethod g reg
;;

let checkAST ast registry =
  let { package = p; type_list = classList; } = ast in
  try
    iterArg checkClass classList registry
    ;
  with
    | MemberNotFound(m) -> print_string ("\027[31mMember not found : " ^ m ^ "\027[0m\n");
    | NotDeferencable(t) -> print_string ("\027[31mType cannot be deferenced : " ^ (Type.stringOf t) ^ "\027[0m\n");
    | PrivateContext(n) -> print_string ("\027[31mThe attribute or method " ^ n ^ " is not accessible in this context\027[0m\n");
    | TypeMismatch(t1,t2) -> print_string ("\027[31mType mismatch exception between " ^ (Type.stringOf t1) ^ " and " ^ (Type.stringOf t2) ^ "\027[0m\n");
    | CannotCompareTypes(t1,t2) -> print_string ("\027[31mCannot compare types " ^ (Type.stringOf t1) ^ " and " ^ (Type.stringOf t2) ^ "\027[0m\n");
    | CannotCast(t1,t2) -> print_string ("\027[31mCannot cast " ^ (Type.stringOf t1) ^ " to " ^ (Type.stringOf t2) ^ "\027[0m\n");
    | BadOperandTypes(t1,t2) -> print_string ("\027[31mBad operand types " ^ (Type.stringOf t1) ^ " and " ^ (Type.stringOf t2) ^ "\027[0m\n");
    | ShouldBeBoolean(t1) -> print_string ("\027[31mExpected type boolean, found " ^ (Type.stringOf t1) ^ "\027[0m\n");
    | _ -> print_string ("\027[31mAn exception of unknown type occured.\027[0m\n");
;;
