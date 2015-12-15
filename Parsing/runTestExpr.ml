let input_file = open_in "testExpr.java" in
let lexbuf = Lexing.from_channel input_file in
let ast = (perform nexttoken lexbuf) in
ast
;;
