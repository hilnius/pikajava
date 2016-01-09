
open TestUtils

let compile_block str =
  let lexbuf = Lexing.from_string str in
  Location.init lexbuf "No filename";
  let result = Parsers.blockDeclaration LexFile.nextToken lexbuf in
  result;;

let print_block b =
  PrintBlock.print_block 1 b;;

let () =
begin
  set_print_function "print_block";
  describe "The block parser";
    it "should parse if statements";
      expect (compile_block "{ if (true) { int int } }") "toBe" (
        Block([
          Statement(IfStatement(
            Bool(true),
            Block([
              Statement(BlockStatement(Block([
                LocalVariableDeclaration(Integer(90))
              ])))
            ]),
            Block([])
          ))
        ])
      );
    end_it ();
    end_describe ();
end;;

