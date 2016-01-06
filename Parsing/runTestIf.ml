#use "blocksAST.ml";;
#use "blocksLexer.ml";;
#use "printAST.ml";;
let input_file = open_in "testIf.java" in
let lexbuf = Lexing.from_channel input_file in
let ast = (formule nexttoken lexbuf) in
printAST(ast)
;;
