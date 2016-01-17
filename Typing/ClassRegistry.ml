open AST

type registry =
  | Registry of packageRegistry list
and packageRegistry =
  | Package of AST.qualified_name * classRegistry list
and classRegistry =
  | Class of string * attributeRegistry list * methodRegistry list
and methodRegistry =
  | Method of string * Type.t * argumentRegistry list
and attributeRegistry =
  | Attribute of string * Type.t
and argumentRegistry =
  | Argument of Type.t
;;

(*
  Example code :
  package a.Test;

  class A {
    int a;
    bool foo(String b) {
      return true;
    }
  }

  Corresponding registry : (after the comment)
*)

let exampleRegisty =
  Package(["a"; "Test"], [
    Class("A", [
      Attribute("a", Primitive(Int))
    ],[
      Method("foo", Primitive(Boolean), [Argument(Ref({ tpath = []; tid = "String" }))])
    ])
  ])
;;


(* build registry functions *)

let buildArgumentRegistry argument = match argument with
  | { final = _; vararg = _; ptype = argType; pident = _ } -> Argument(argType)
;;

let buildMethodRegistry meth = match meth with
  | { mmodifiers = _; mname = name; mreturntype = returnType; margstype = arguments; mthrows = _; mbody = _ } ->
    Method(name, returnType, List.map buildArgumentRegistry arguments)
;;

let buildAttributeRegistry attribute = match attribute with
  | { amodifiers = _; aname = name; atype = argType; adefault = _ } ->
    Attribute(name, argType)
;;

let buildClassRegistry classes = match classes with
  | { modifiers = _; id = className; info = { cparent = _; cattributes = attributes; cinits = _; cconsts = _; cmethods = methods; cloc = _ } } ->
    Class(className, List.map buildAttributeRegistry attributes, List.map buildMethodRegistry methods)
;;

let buildPackageRegistry ast =
  (* cf AST.ml *)
  let { package = packageDeclaration; type_list = classList; } = ast in
  Package(["a";"Test"], List.map buildClassRegistry classList)
;;


(* stringOf functions *)
let currentTabs = ref 0;;

let rec generateTabs x = match x with
  | 0 -> ""
  | i when i > 0 -> "  " ^ (generateTabs (i - 1))
;;

let getTabs () =
  generateTabs !currentTabs
;;

let increaseTab () =
  currentTabs := (!currentTabs + 1)
;;

let decreaseTab () =
  currentTabs := (!currentTabs - 1)
;;

let rec iterToString f l separator = match l with
  | [] -> ""
  | h::[] -> f h
  | h::t -> (f h) ^ separator ^ (iterToString f t separator)
;;

let iterTabbed f l =
  increaseTab ();
  let a = iterToString f l "" in
  decreaseTab();
  a
;;

let stringOfArgument arg = match arg with
  | Argument(argType) -> Type.stringOf argType
;;

let stringOfMethod attr = match attr with
  | Method(name, returnType, arguments) ->
    (getTabs ()) ^ (Type.stringOf returnType) ^ " " ^ name ^ "(" ^ (iterToString stringOfArgument arguments ", ") ^ ");\n"
;;

let stringOfAttribute attr = match attr with
  | Attribute(name, argType) ->
    (getTabs ()) ^ (Type.stringOf argType) ^ " " ^ name ^ ";\n"
;;

let stringOfClass c = match c with
  | Class(className, attributes, methods) ->
    (getTabs ()) ^ (className) ^ " {\n" ^ (iterTabbed stringOfAttribute attributes) ^ (iterTabbed stringOfMethod methods) ^ (getTabs ()) ^ ("}\n")
;;

let stringOfPackageRegistry r = match r with
  | Package(name, classes) ->
    (getTabs ()) ^ ("Package ") ^ (List.hd name) ^ " {\n" ^ (iterTabbed stringOfClass classes) ^ (getTabs ()) ^ ("}\n")
;;

let printPackage p =
  print_string (stringOfPackageRegistry p)
;;


