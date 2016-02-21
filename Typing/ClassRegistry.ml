open AST
open Exceptions

type registry =
  | Registry of packageRegistry list
and packageRegistry =
  | RPackage of AST.qualified_name * classRegistry list
and classRegistry =
  | RClass of string * attributeRegistry list * methodRegistry list
and methodRegistry =
  | RMethod of string * Type.t * argumentRegistry list
and attributeRegistry =
  | RAttribute of string * Type.t
and argumentRegistry =
  | RArgument of Type.t
;;

type methodOrAttributeSignature =
  | MethodSignature of (Type.t * argumentRegistry list)
  | AttributeSignature of Type.t

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
  RPackage(["a"; "Test"], [
    RClass("A", [
      RAttribute("a", Primitive(Int))
    ],[
      RMethod("foo", Primitive(Boolean), [RArgument(Ref({ tpath = []; tid = "String" }))])
    ])
  ])
;;

let rec findClass classes className = match classes with
  | [] -> raise (ClassNameNotFound(className))
  | (RClass(n, _, _))::_ when n = className -> List.hd classes
  | (RClass(n, _, _))::t -> findClass t className
;;

let getClassMethod className member arguments registry =
  let rec findMethod m = match m with
    | [] -> raise (MemberNotFound(member))
    | (RMethod(n, t1, args))::t when member = n -> t1
    | h::t -> findMethod t
  in
  match registry with
    | RPackage(_, classes) -> let RClass(_,_,m) = (findClass classes className) in findMethod m
;;

let getClassAttribute className member registry =
  let rec findAttribute a = match a with
    | [] -> raise (MemberNotFound(member))
    | (RAttribute(n, t1))::t when member = n -> t1
    | h::t -> findAttribute t
  in
  match registry with
    | RPackage(_, classes) -> let RClass(_,a,_) = (findClass classes className) in findAttribute a
;;

(* build registry functions *)

let buildArgumentRegistry argument = match argument with
  | { final = _; vararg = _; ptype = argType; pident = _ } -> RArgument(argType)
;;

let buildMethodRegistry meth = match meth with
  | { mmodifiers = _; mname = name; mreturntype = returnType; margstype = arguments; mthrows = _; mbody = _ } ->
    RMethod(name, returnType, List.map buildArgumentRegistry arguments)
;;

let buildAttributeRegistry attribute = match attribute with
  | { amodifiers = _; aname = name; atype = argType; adefault = _ } ->
    RAttribute(name, argType)
;;

let buildClassRegistry classes = match classes with
  | { modifiers = _; id = className; info = Class({ cparent = _; cattributes = attributes; cinits = _; cconsts = _; cmethods = methods; cloc = _ }) } ->
    RClass(className, List.map buildAttributeRegistry attributes, List.map buildMethodRegistry methods)
;;

let buildPackageRegistry ast =
  (* cf AST.ml *)
  let { package = packageDeclaration; type_list = classList; } = ast in
  RPackage(["a";"Test"], List.map buildClassRegistry classList)
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
  | RArgument(argType) -> Type.stringOf argType
;;

let stringOfMethod attr = match attr with
  | RMethod(name, returnType, arguments) ->
    (getTabs ()) ^ (Type.stringOf returnType) ^ " " ^ name ^ "(" ^ (iterToString stringOfArgument arguments ", ") ^ ");\n"
;;

let stringOfAttribute attr = match attr with
  | RAttribute(name, argType) ->
    (getTabs ()) ^ (Type.stringOf argType) ^ " " ^ name ^ ";\n"
;;

let stringOfClass c = match c with
  | RClass(className, attributes, methods) ->
    (getTabs ()) ^ (className) ^ " {\n" ^ (iterTabbed stringOfAttribute attributes) ^ (iterTabbed stringOfMethod methods) ^ (getTabs ()) ^ ("}\n")
;;

let stringOfPackageRegistry r = match r with
  | RPackage(name, classes) ->
    (getTabs ()) ^ ("Package ") ^ (List.hd name) ^ " {\n" ^ (iterTabbed stringOfClass classes) ^ (getTabs ()) ^ ("}\n")
;;

let printPackage p =
  print_string (stringOfPackageRegistry p)
;;


