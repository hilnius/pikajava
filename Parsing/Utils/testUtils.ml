let currentDescribe = ref "";;
let currentIt = ref "";;
let assertionsPassed = ref 0;;
let assertionsFailed = ref 0;;
let failures = ref [];;
let printFunctionName = ref "";;

let set_print_function fn =
  printFunctionName := fn;;

let get_print_function () =
  match (!printFunctionName) with
  | "print_block" -> PrintBlock.print_block 1
  (* Add your print functions here, used for debug output *)
;;

let describe str =
  currentDescribe := str;;

let it str =
  assertionsPassed := 0;
  assertionsFailed := 0;
  currentIt := str;;

let print_errors l = match l with
  | [] -> ()
  | (a,b)::q -> begin
    print_string "  \027[33mExpects:\027[0m\n";
    get_print_function () a;
    print_string "  \027[33mTo be:\027[0m\n";
    get_print_function () b;
  end;;

let end_it qsd =
  if !assertionsFailed = 0 then begin
    print_string "\027[32m";
    print_string (!currentDescribe);
    print_string " ";
    print_string (!currentIt);
    print_string " - ";
    print_int !assertionsPassed;
    print_string " passed";
    print_string "\027[0m\n";
  end else begin
    print_string "\027[31m";
    print_string (!currentDescribe);
    print_string " ";
    print_string (!currentIt);
    print_string " - \027[33m";
    print_int (!assertionsPassed);
    print_string " passed\027[31m, ";
    print_int (!assertionsFailed);
    print_string " failed";
    print_string "\027[0m\n";
    print_errors (!failures);
  end;;

let end_describe () = print_string "\n";;

let toBe value result =
  value = result;;

let toBeDifferent value result =
  value <> result;;

let expect value comparatorName result =
  let comparator = match comparatorName with
    | "toBe" -> toBe
    | "toBeDifferent" -> toBeDifferent
    | _ -> toBe
  in
  if comparator value result then
    assertionsPassed := (!assertionsPassed + 1)
  else begin
    assertionsFailed := (!assertionsFailed + 1);
    failures := ((value, result)::(!failures));
  end
;;


