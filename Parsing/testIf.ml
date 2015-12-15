
let input_file = open_in "testIf.java" in
let lexbuf = Lexing.from_channel input_file in
printAST (formule nexttoken lexbuf);;
