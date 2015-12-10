#use "blocksAST.ml";;
#use "blocksLexer.ml";;

let read_file filename =
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := ((input_line chan) ^ !lines)
    done;
    !lines
  with End_of_file ->
    close_in chan;
    !lines
in

let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    begin
      try
        formule nexttoken lexbuf;
        ()
      with _ ->
        let startPos, endPos = (Lexing.lexeme_start_p lexbuf).pos_cnum, (Lexing.lexeme_end_p lexbuf).pos_cnum in
        print_string ("Error while reading lexeme at characters " ^ (string_of_int startPos) ^ "-" ^ (string_of_int endPos) ^ " : \"" ^ (String.sub (read_file file) startPos (endPos - startPos)) ^ "\"\r\n")
    end;
    close_in (input_file)
  with Sys_error s ->
    print_endline ("Can’t find file ’" ^ file ^ "’")
in

compile "testIf.ml";;
