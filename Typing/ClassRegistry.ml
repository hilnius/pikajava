open AST
open Exceptions

(*
A registry is an object that contains the packages, classes with their attributes, methods and argments.
This is build before typing statements. It basically contains the return types of methods, types of
attributes and types of arguments.

This can also be used to read all files necessary for compiling, and create a registry of all possible
classes and their methods and attributes.

That way, the typer will query the registry everytime it encounters an attribute / method call, and
the registry will be able to tell the typer what is the return type of what he's using.

getClassMethod and getClassAttribute are functions that do exactly that (take a class and a method or
attribute name, and return the type of this method / attribute).
*)

type registry =
  | Registry of packageRegistry list
and packageRegistry =
  | RPackage of AST.qualified_name * classRegistry list
and classRegistry = (* name, parent, attributes, methods *)
  | RClass of string * Type.ref_type option * attributeRegistry list * methodRegistry list
and methodRegistry =
  | RMethod of string * Type.t * argumentRegistry list * modifier list
and attributeRegistry =
  | RAttribute of string * Type.t * modifier list
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

let rec hasModifier modifiers modifier = match modifiers with
  | [] -> false
  | h::t when h = modifier -> true
  | h::t -> hasModifier t modifier

let objectClassRegistry = RClass("Object", None, [],[])
;;

let rec findClass classes className = match classes with
  | [] -> raise (ClassNameNotFound(className))
  | (RClass(n, _, _, _))::_ when n = className -> List.hd classes
  | (RClass(n, _, _, _))::t -> findClass t className
;;

let getClassParent className registry =
  let rec findParent a = match a with
    | [] -> None
    | (RClass(n, parent, _, _))::_ when n = className -> parent
    | h::t -> findParent t
  in
  match registry with
    | RPackage(_, classes) -> findParent classes
;;

let getClassMethodAll className member arguments registry =
  let rec findMethod m = match m with
    | [] -> raise (MemberNotFound(member))
    | (RMethod(n, t1, args, modif))::t when member = n -> RMethod(n, t1, args, modif)
    | h::t -> findMethod t
  in
  match registry with
    | RPackage(_, classes) -> let RClass(_,_,_,m) = (findClass classes className) in findMethod m
;;

let getClassMethod className member arguments registry =
  match getClassMethodAll className member arguments registry with
    | RMethod(n, t1, args, modif) when member = n -> t1
;;

let getClassAttribute className member registry =
  let rec findAttribute a = match a with
    | [] -> raise (MemberNotFound(member))
    | (RAttribute(n, t1, modif))::t when member = n -> t1
    | h::t -> findAttribute t
  in
  match registry with
    | RPackage(_, classes) -> let RClass(_,_,a,_) = (findClass classes className) in findAttribute a
;;

let rec getClassMethodParent classCaller className member arguments registry = match registry with
  | RPackage(_, classes) -> let RClass(_,_,a,_) = (findClass classes className) in
    try
      let m = getClassMethodAll className member arguments registry in
        match m with
	  | RMethod(n, t1, args, modif) -> begin if hasModifier modif Private then
              if classCaller <> className then raise(PrivateContext(n)) else m
            else
              m
          end;
    with
      | MemberNotFound(member) -> let parent = getClassParent className registry in
        match parent with
          | None -> raise (MemberNotFound(member))
          | Some(p) -> getClassMethodParent classCaller p.tid member arguments registry
      ;
    ;
;;

(* build registry functions *)

let buildArgumentRegistry argument = match argument with
  | { final = _; vararg = _; ptype = argType; pident = _ } -> RArgument(argType)
;;

let buildMethodRegistry meth = match meth with
  | { mmodifiers = modifiers; mname = name; mreturntype = returnType; margstype = arguments; mthrows = _; mbody = _ } ->
    RMethod(name, returnType, List.map buildArgumentRegistry arguments, modifiers)
;;

let buildAttributeRegistry attribute = match attribute with
  | { amodifiers = modifiers; aname = name; atype = argType; adefault = _ } ->
    RAttribute(name, argType, modifiers)
;;

let buildClassRegistry classes = match classes with
  | { modifiers = _; id = className; info = Class({ cparent = parent; cattributes = attributes; cinits = _; cconsts = _; cmethods = methods; cloc = _ }) } ->
    RClass(className, Some(parent), List.map buildAttributeRegistry attributes, List.map buildMethodRegistry methods)
;;

let buildPackageRegistry ast =
  (* cf AST.ml *)
  let { package = packageDeclaration; type_list = classList; } = ast in
  RPackage(["root"], objectClassRegistry::(List.map buildClassRegistry classList))
;;

(* Class Hierarchy *)

type classHierarchy =
  | ClassHierarchy of string * classHierarchy list
  | NoneClassHierarchy

type delayedClassHierarchy =
  | DelayedClassHierarchy of string * string

let classTree : classHierarchy list ref = ref [ ClassHierarchy("Object", []) ];;

let delayedList : delayedClassHierarchy list ref = ref [];;

let rec insertClassNode tree name parent = match tree with
  | [] -> []
  | ClassHierarchy(cname, clist)::t when cname = parent -> ClassHierarchy(parent, ClassHierarchy(name, [])::clist)::t
  | ClassHierarchy(cname, clist)::t -> ClassHierarchy(cname, insertClassNode clist name parent)::(insertClassNode t name parent)
;;

let rec findNode tree name = match tree with
  | [] -> NoneClassHierarchy
  | h::t -> match h with
    | ClassHierarchy(cname, clist) when cname = name -> h
    | ClassHierarchy(cname, clist) -> findNode (t @ clist) name
;;

let rec registerDelayedClasses d = match d with
  | [] -> []
  | DelayedClassHierarchy(name, parent)::t -> match findNode !classTree parent with
    | NoneClassHierarchy -> DelayedClassHierarchy(name, parent)::registerDelayedClasses t
    | _ -> begin
    classTree := insertClassNode !classTree name parent;
    registerDelayedClasses t
  end

let registerClassParent name parent = match findNode !classTree parent with
  | NoneClassHierarchy -> begin
    delayedList := DelayedClassHierarchy(name, parent)::!delayedList
  end
  | _ -> begin
    classTree := insertClassNode !classTree name parent;
    (* adding delayed classes *)
    delayedList := registerDelayedClasses !delayedList
  end
;;

let rec stringOfClassTree tree = match tree with
  | [] -> ""
  | ClassHierarchy(cname, clist)::t -> cname ^ "[ " ^ stringOfClassTree clist ^ " ] " ^ stringOfClassTree t
;;

let checkClassInstanceOf name parent = match findNode !classTree parent with
  | NoneClassHierarchy -> false
  | node -> match findNode [node] name with
    | NoneClassHierarchy -> false
    | _ -> true
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
  | RMethod(name, returnType, arguments, modif) ->
    (getTabs ()) ^ (Type.stringOf returnType) ^ " " ^ name ^ "(" ^ (iterToString stringOfArgument arguments ", ") ^ ");\n"
;;

let stringOfAttribute attr = match attr with
  | RAttribute(name, argType, modif) ->
    (getTabs ()) ^ (Type.stringOf argType) ^ " " ^ name ^ ";\n"
;;

let stringOfClass c = match c with
  | RClass(className, _, attributes, methods) ->
    (getTabs ()) ^ (className) ^ " {\n" ^ (iterTabbed stringOfAttribute attributes) ^ (iterTabbed stringOfMethod methods) ^ (getTabs ()) ^ ("}\n")
;;

let stringOfPackageRegistry r = match r with
  | RPackage(name, classes) ->
    (getTabs ()) ^ ("Package ") ^ (List.hd name) ^ " {\n" ^ (iterTabbed stringOfClass classes) ^ (getTabs ()) ^ ("}\n")
;;

let printPackage p =
  print_string (stringOfPackageRegistry p)
;;


