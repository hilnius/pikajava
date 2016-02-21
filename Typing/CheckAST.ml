open Type
open AST
open Exceptions

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
  | _ -> t1 == t2
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
  | Type.Primitive(Type.Int) | Type.Primitive(Type.Long) |  Type.Primitive(Type.Short) -> true
  | _ -> false
;;

let checkExpression e = match e with
  (*| New(name, identifiers, arguments) ->
  | Call of expression * string * expression list
  | Attr of expression * string
  | If of expression * expression * expression *)
  | Val(value) -> ()
  (* | Name of string
  | ArrayInit of expression list*)
  | AssignExp(e1, aop, e2) -> begin match aop with
      | Assign | Ass_add | Ass_sub | Ass_mul | Ass_div -> if not(compareTypes (extractSome e1.etype) (extractSome e2.etype)) then raise (CannotCompareTypes(extractSome e1.etype, extractSome e2.etype))
      | Ass_mod -> if not(checkPrimitiveNotBoolean (extractSome e1.etype) && checkPrimitiveNotBoolean (extractSome e2.etype)) then raise (BadOperandTypes(extractSome e1.etype, extractSome e2.etype))
      | Ass_shl | Ass_shr | Ass_shrr -> if not(checkIntegerKind (extractSome e1.etype) && checkIntegerKind (extractSome e2.etype)) then raise (BadOperandTypes(extractSome e1.etype, extractSome e2.etype))
    end;
  (*| Post of expression * postfix_op *)
  | Op(e1, op, e2) -> begin match op with
      | Op_eq | Op_ne | Op_gt | Op_lt | Op_ge | Op_le -> if not(compareTypes (extractSome e1.etype) (extractSome e2.etype)) then raise (CannotCompareTypes(extractSome e1.etype, extractSome e2.etype))
      | Op_cor | Op_cand -> begin
         if not (checkBoolean (extractSome e1.etype)) then raise (ShouldBeBoolean(extractSome e1.etype));
         if not (checkBoolean (extractSome e2.etype)) then raise (ShouldBeBoolean(extractSome e2.etype))
      end;
    end;
  | CondOp(e1, e2, e3) -> begin
      if not (checkBoolean (extractSome e1.etype)) then raise (ShouldBeBoolean(extractSome e1.etype));
      if not (compareTypes (extractSome e1.etype) (extractSome e2.etype)) then raise (CannotCompareTypes(extractSome e1.etype, extractSome e2.etype))
    end;
  (*| Cast of expression * expression
  | Instanceof of expression * expression *)
  | _ -> ()
;;

let checkVarDecl v = match v with
  | (t, varName, None) -> ()
  | (t, varName, Some(init)) ->
    checkExpression (init.edesc);
    let t2 = extractSome init.etype in
    if (t <> t2) then begin
      raise (TypeMismatch(t, t2))
    end
;;

let rec checkStatement s = match s with
  | VarDecl(l) -> List.iter checkVarDecl l
  | Block(sl) -> List.iter checkStatement sl
  (*| Nop
  | While of expression * statement
  | For of (Type.t * string * expression option) list * expression option * expression list * statement *)
  | If(e1, ifSt, None) -> checkExpression e1.edesc; if extractSome (e1.etype) <> Primitive(Boolean) then raise (ShouldBeBoolean(extractSome e1.etype)); checkStatement ifSt;
  | If(e1, ifSt, Some(elseSt)) -> checkExpression e1.edesc; if extractSome (e1.etype) <> Primitive(Boolean) then raise (ShouldBeBoolean(extractSome e1.etype)); checkStatement ifSt; checkStatement elseSt
  (*| Return of expression option
  | Throw of expression
  | Try of statement list * (argument * statement list) list * statement list
  | Expr of expression *)
  | _ -> ()
;;

let checkMethod meth = match meth with
  | { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = f } ->
    List.iter checkStatement f
;;

let checkClass cl = match cl with
  | { modifiers = a; id = b; info = Class({ cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = g; cloc = h }) }->
    List.iter checkMethod g
;;

let checkAST ast =
  let { package = p; type_list = classList; } = ast in
  try
    List.iter checkClass classList;
  with
    | TypeMismatch(t1,t2) -> print_string ("\027[31mType mismatch exception between " ^ (Type.stringOf t1) ^ " and " ^ (Type.stringOf t2) ^ "\027[0m\n");
    | CannotCompareTypes(t1,t2) -> print_string ("\027[31mCannot compare types " ^ (Type.stringOf t1) ^ " and " ^ (Type.stringOf t2) ^ "\027[0m\n");
    | BadOperandTypes(t1,t2) -> print_string ("\027[31mBad operand types " ^ (Type.stringOf t1) ^ " and " ^ (Type.stringOf t2) ^ "\027[0m\n");
    | ShouldBeBoolean(t1) -> print_string ("\027[31mExpected type boolean, found " ^ (Type.stringOf t1) ^ "\027[0m\n");
    | _ -> print_string ("\027[31mAn exception of unknown type occured.\027[0m\n");
;;
