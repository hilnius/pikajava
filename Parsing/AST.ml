type argument = {
    final : bool;
    vararg : bool;
    ptype : Type.t;
    pident : string;
  }
    
type value = 
  | String of string
  | Int of string
  | Float of string
  | Char of char
  | Null
  | Boolean of bool

type postfix_op =
  | Incr
  | Decr
      
type assign_op =
  | Assign  
  | Ass_add 
  | Ass_sub 
  | Ass_mul 
  | Ass_div 
  | Ass_mod 
  | Ass_shl 
  | Ass_shr 
  | Ass_shrr
  | Ass_and 
  | Ass_xor 
  | Ass_or  

type infix_op =
  | Op_cor  
  | Op_cand 
  | Op_or   
  | Op_and  
  | Op_xor  
  | Op_eq   
  | Op_ne   
  | Op_gt   
  | Op_lt   
  | Op_ge   
  | Op_le   
  | Op_shl  
  | Op_shr  
  | Op_shrr 
  | Op_add  
  | Op_sub  
  | Op_mul  
  | Op_div  
  | Op_mod  

type expression_desc = 
  | New of string list * expression list
  | Call of expression * string * expression list
  | Attr of expression * string
  | If of expression * expression * expression
  | Val of value
  | Name of string
  | ArrayInit of expression list
  | AssignExp of expression * assign_op * expression
  | Post of expression * postfix_op
  | Op of expression * infix_op * expression
  | CondOp of expression * expression * expression
  | Cast of expression * expression
  | Instanceof of expression * expression

and expression = 
    {
      edesc : expression_desc;
(*      eloc : Location.t;
      mutable etype : Type.t option;*)
    }

type modifier =
  | Abstract 
  | Public   
  | Protected
  | Private  
  | Static   
  | Final    
  | Strictfp
  | Transient 
  | Volatile 
  | Synchronized 
  | Native 

type astattribute = {
      mutable amodifiers : modifier list;
      aname : string;
      atype : Type.t;
      adefault : expression option;
      (*      aloc : Location.t;*)
    }
      



type qualified_name = string list

type statement =
  | VarDecl of (Type.t * string * expression option) list
  | Block of statement list
  | Nop
  | While of expression * statement
  | For of (Type.t * string * expression option) list * expression option * expression list * statement
  | If of expression * statement * statement option
  | Return of expression option
  | Throw of expression
  | Try of statement list * (argument * statement list) list * statement list
  | Expr of expression

type astmethod = {
    mutable mmodifiers : modifier list;
    mname : string;
    mreturntype : Type.t;
    margstype : argument list;
    mthrows : Type.ref_type list;
    mbody : statement list;
    (*      mloc : Location.t;*)
  }
type astconst = {
    mutable cmodifiers : modifier list;
    cname : string;
    cargstype : argument list;
    cthrows : Type.ref_type list;
    cbody : statement list;
    (*      mloc : Location.t;*)
  }

and astclass = {
    cparent : Type.ref_type;
    cattributes : astattribute list;
    cinits : initial list;
    cconsts : astconst list;
    cmethods : astmethod list;
    cloc : Location.t;
  }

and initial = {
    static : bool ;
    block : statement list
  }

and asttype =
  {
    modifiers : modifier list;
    id : string;
    info : astclass;
  }

type t = {
    package : qualified_name option;
    type_list : asttype list;
  }


(* Printing functions *)

let string_of_value = function
  | String s -> "\""^s^"\""
  | Boolean b -> string_of_bool b
  | Int s -> s
  | Float f -> f
  | Char c -> String.make 1 c
  | Null -> "null"
	      
let string_of_assign_op = function
  | Assign  -> "="
  | Ass_add -> "+="
  | Ass_sub -> "-="
  | Ass_mul -> "*="
  | Ass_div -> "/="
  | Ass_mod -> "%="
  | Ass_shl -> "<<="
  | Ass_shr -> ">>="
  | Ass_shrr-> ">>>="
  | Ass_and -> "&="
  | Ass_xor -> "^="
  | Ass_or  -> "|="

let string_of_infix_op = function
  | Op_cor   -> "||"
  | Op_cand  -> "&&"
  | Op_or    -> "|"
  | Op_and   -> "&"
  | Op_xor   -> "^"
  | Op_eq    -> "=="
  | Op_ne    -> "!="
  | Op_gt    -> ">"
  | Op_lt    -> "<"
  | Op_ge    -> ">="
  | Op_le    -> "<="
  | Op_shl   -> "<<"
  | Op_shr   -> ">>"
  | Op_shrr  -> ">>>"
  | Op_add   -> "+"
  | Op_sub   -> "-"
  | Op_mul   -> "*"
  | Op_div   -> "/"
  | Op_mod   -> "%"

let rec string_of_expression_desc = function
  | New(n,al) -> 
      "new "^(String.concat "." n)^"("^
      (String.concat "," (List.map string_of_expression al))^
      ")"
  | If(c,e1,e2) -> 
      "if "^(string_of_expression c)^" { "^
      (string_of_expression e1)^" } else { "^(string_of_expression e2)^" }"
  | Call(r,m,al) -> 
      (string_of_expression r)^
      "."^m^"("^
      (String.concat "," (List.map string_of_expression al))^
	")"
  | Attr(r,a) ->
      (string_of_expression r)^
      "."^a
  | Val v -> string_of_value v
  | Name s -> s
  | AssignExp(e1,op,e2) ->
      (string_of_expression e1)^(string_of_assign_op op)^(string_of_expression e2)
  | Op(e1,op,e2) ->
      (string_of_expression e1)^(string_of_infix_op op)^(string_of_expression e2)
  | CondOp(e1,e2,e3) ->
     (string_of_expression e1)^"?"^(string_of_expression e2)^":"^(string_of_expression e3)
  | ArrayInit el ->
     "{"^(ListII.concat_map "," string_of_expression el)^"}"
  | Cast(t,e) ->
      "("^(string_of_expression t)^") "^(string_of_expression e)
  | Instanceof(e,t) ->
     (string_of_expression e)^" instanceof "^(string_of_expression t)
  | Post(e,Incr) -> (string_of_expression e)^"++"
  | Post(e,Decr) -> (string_of_expression e)^"--"

and string_of_expression e = 
  let s = string_of_expression_desc e.edesc in
  s(*
    match e.etype with
      | None -> s
      | Some t -> "("^s^" : "^(Type.stringOf t)^")"*)

let print_attribute a =
  print_string "  ";
  print_string ((Type.stringOf a.atype)^" "^a.aname);
  (match a.adefault with
    | None -> ()
    | Some e -> print_string(" = "^(string_of_expression e)));
  print_endline ";"

let stringOf_arg a =
  (if a.final then "final " else "")^
    (Type.stringOf a.ptype)^
      (if a.vararg then "..." else "")^
	" "^a.pident

let stringOf_modifier = function
  | Abstract  -> "abstract" 
  | Public    -> "public"   
  | Protected -> "protected"
  | Private   -> "private"  
  | Static    -> "static"   
  | Final     -> "final"    
  | Strictfp  -> "strictfp" 
  | Transient    -> "transient"   
  | Volatile     -> "volatile"    
  | Synchronized -> "synchronized"
  | Native       -> "native"      

let rec print_statement tab = function
  | VarDecl dl ->
     List.iter (fun (t,id,init) ->
		print_string(tab^(Type.stringOf t)^" "^id);
		(match init with
		| None -> ()
		| Some e -> print_string (" "^(string_of_expression e)));
		print_endline ";"
	       ) dl
  | Block b ->
     print_endline(tab^"{");
     List.iter (print_statement (tab^"  ")) b;
     print_endline(tab^"}")
  | Nop -> print_endline(tab^";")
  | Expr e -> print_endline(tab^(string_of_expression e)^";")
  | Return None -> print_endline(tab^"return;")
  | Return Some(e) -> print_endline(tab^"return "^(string_of_expression e)^";")
  | Throw e -> print_endline(tab^"throw "^(string_of_expression e)^";")
  | While(e,s) ->
     print_endline(tab^"while ("^(string_of_expression e)^") {");
     print_statement (tab^"  ") s;
     print_endline(tab^"}")
  | If(e,s,None) ->
     print_endline(tab^"if ("^(string_of_expression e)^") {");
     print_statement (tab^"  ") s;
     print_endline(tab^"}")
  | If(e,s1,Some s2) ->
     print_endline(tab^"if ("^(string_of_expression e)^") {");
     print_statement (tab^"  ") s1;
     print_endline(tab^"} else {");
     print_statement (tab^"  ") s2;
     print_endline(tab^"}")
  | For(fil,eo,el,s) ->
     print_string(tab^"for (");
     List.iter (fun (t,s,eo) ->
		print_string((Type.stringOf t)^" "^s);
		(match eo with
		| None -> ()
		| Some e -> print_string (" "^(string_of_expression e)))) fil;
     print_string ";";
     (match eo with
      | None -> ()
      | Some e -> print_string (" "^(string_of_expression e)));
     print_string ";";
     print_string(ListII.concat_map "," string_of_expression el);
     print_endline ") {";
     print_statement (tab^"  ") s;
     print_endline(tab^"}")
(*  Manque le 
  | Try of statement list * (argument * statement list) list * statement list
 *)
     
  
and print_method m =
  print_string "  ";
  print_string((Type.stringOf m.mreturntype)^" "^m.mname^"(");
  print_string(ListII.concat_map "," stringOf_arg m.margstype);
  print_string(")");
  print_string(" "^ListII.concat_map "," Type.stringOf_ref m.mthrows);
  print_endline(" {");
  List.iter (print_statement "  ") m.mbody;
  print_endline("  }")

and print_const c =
  print_string ("  "^c.cname^"(");
  print_string(ListII.concat_map "," stringOf_arg c.cargstype);
  print_endline(") {");
  List.iter (print_statement "  ") c.cbody;
  print_endline("  }")

and print_class c =
  print_endline(" extends "^(Type.stringOf_ref c.cparent)^" {");
  List.iter print_attribute c.cattributes;
  List.iter print_const c.cconsts;
  List.iter print_method c.cmethods;
  print_endline "}";
  print_newline()

and print_type t =
  if t.modifiers != [] then
    print_string ((ListII.concat_map " " stringOf_modifier t.modifiers)^" ");
  print_string ("class "^t.id); print_class t.info
  
let print_package p =
  print_string "package ";
  print_endline (String.concat "." p)
	       
let print_program p =
  match p.package with
  | None -> ()
  | Some pack -> print_package pack ;
  List.iter print_type p.type_list
