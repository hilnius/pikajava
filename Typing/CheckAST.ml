open AST
open Exceptions

let extractSome a = match a with
  | None -> raise UntypedExpression;
  | Some(b) -> b;
;;

let checkExpression e = match e with
  (* | New of string list * expression list
  | Call of expression * string * expression list
  | Attr of expression * string
  | If of expression * expression * expression *)
  | Val(value) -> ()
  (* | Name of string
  | ArrayInit of expression list
  | AssignExp of expression * assign_op * expression
  | Post of expression * postfix_op *)
  | Op(e1, op, e2) -> begin match op with
      | Op_eq -> if extractSome e1.etype <> extractSome e2.etype then raise (CannotCompareTypes(extractSome e1.etype, extractSome e2.etype))
    end;
  (*| CondOp of expression * expression * expression
  | Cast of expression * expression
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
    | ShouldBeBoolean(t1) -> print_string ("\027[31mExpected type boolean, found " ^ (Type.stringOf t1) ^ "\027[0m\n");
    | _ -> print_string ("\027[31mAn exception of unknown type occured.\027[0m\n");
;;
