open AST

exception UntypedExpression;;
exception TypeMismatch of (Type.t * Type.t);;

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
  | Post of expression * postfix_op
  | Op of expression * infix_op * expression
  | CondOp of expression * expression * expression
  | Cast of expression * expression
  | Instanceof of expression * expression *)
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

let checkStatement s = match s with
  | VarDecl(l) -> List.iter checkVarDecl l
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

let checkMethod meth = match meth with
  | { mmodifiers = a; mname = b; mreturntype = c; margstype = d; mthrows = e; mbody = f } ->
    List.iter checkStatement f
;;

let checkClass cl = match cl with
  | { modifiers = a; id = b; info = { cparent = c; cattributes = d; cinits = e; cconsts = f; cmethods = g; cloc = h } }->
    List.iter checkMethod g
;;

let checkAST ast =
  let { package = p; type_list = classList; } = ast in
  try
    List.iter checkClass classList;
  with
    | TypeMismatch(t1,t2) -> print_string ("\027[31mType mismatch exception : " ^ (Type.stringOf t1) ^ " " ^ (Type.stringOf t2) ^ "\027[0m\n");
    | _ -> print_string ("\027[31mAn exception of unknown type occured.\027[0m\n");
;;
