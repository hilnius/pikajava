open Lexing
open String

type t =
    {
      loc_start: position;
      loc_end: position;
    }

let none =
  {
    loc_start = dummy_pos;
    loc_end = dummy_pos;
  }

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
}

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let incr_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
  {
    pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
  }

let symbol_loc startpos endpos =
  {
    loc_start = startpos;
    loc_end = endpos;
  }

let print loc =
  let debut = loc.loc_start
  and fin = loc.loc_end in
  print_string ("File \"" ^ debut.pos_fname ^ "\", ");
  if (debut.pos_lnum = fin.pos_lnum) then
    begin
      print_string "line ";
      print_int debut.pos_lnum;
      print_string ", characters ";
      print_int (debut.pos_cnum - debut.pos_bol);
      print_string "-";
      print_int (fin.pos_cnum - fin.pos_bol)
    end
  else
    begin
      print_string "from line ";
      print_int debut.pos_lnum;
      print_string " character ";
      print_int (debut.pos_cnum - debut.pos_bol);
      print_string " to line ";
      print_int fin.pos_lnum;
      print_string " character ";
      print_int (fin.pos_cnum - fin.pos_bol)
    end

let read_token filename lnum posStart posEnd =
  let chan = open_in filename in
  let myStr = (Bytes.create 100) in
  try
    begin
    for i = 2 to lnum do
      input_line chan
    done;
    let rightLine = input_line chan in
      close_in chan;
      String.sub  rightLine posStart (posEnd - posStart)
    end
  with End_of_file ->
    close_in chan;
    "";;

let print_token loc =
  let debut = loc.loc_start
  and fin = loc.loc_end in
  if (debut.pos_lnum = fin.pos_lnum) then
      print_string (read_token debut.pos_fname debut.pos_lnum (debut.pos_cnum - debut.pos_bol) (fin.pos_cnum - fin.pos_bol)  );;

let print_token_full loc =
  begin
    print_string "\"";
    print_token loc;
    print_string "\" in ";
    print loc;
    print_newline ();
  end;;
