
type variableDeclaration = Integer of int
type expression = Bool of bool
type classDeclaration = Class

type block = Block of blockStatement list
and blockStatement =
    ClassDeclaration of classDeclaration
  | LocalVariableDeclaration of variableDeclaration
  | Statement of statement
and statement =
    IfStatement of (expression * block * block)
  | ForStatement of (statement * expression * expression * block)
  | WhileStatement of (expression * block)
  | BlockStatement of block
  | EmptyStatement
