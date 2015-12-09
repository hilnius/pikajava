exception Error

type token = 
  | SEMICOLON
  | OPEN_PARENTHESIS
  | OPEN_BRACKET
  | INTEGER of (
# 71 "blocksAST.mly"
       (int)
# 11 "blocksAST.ml"
)
  | IF
  | EOF
  | ELSE
  | CLOSE_PARENTHESIS
  | CLOSE_BRACKET
  | BOOLEAN of (
# 72 "blocksAST.mly"
       (bool)
# 21 "blocksAST.ml"
)

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState21
  | MenhirState10
  | MenhirState7
  | MenhirState1
  | MenhirState0


# 1 "blocksAST.mly"
  

	 (*type statementExpression =
		  Assignment of assignment
		 | PreIncrementExpression of preIncrementExpression
		 | PreDecrementExpression of preDecrementExpression
		 | PostIncrementExpression of postIncrementExpression
		 | PostDecrementExpression of postDecrementExpression
		 | MethodInvocation of methodInvocation
		 | ClassInstanceCreationExpression of classInstanceCreationExpression

	 type expressionStatement = StatementExpression of statementExpression

	type statementWithoutTrailingSubstatement =
		  Block of block
		 | EmptyStatement of emptyStatement
		 | ExpressionStatement of expressionStatement
		 | AssertStatement of assertStatement
		 | SwitchStatement of switchStatement
		 | DoStatement of doStatement
		 | BreakStatement of breakStatement
		 | ContinueStatement of continueStatement
		 | ReturnStatement of returnStatement
		 | SynchronizedStatement of synchronizedStatement
		 | ThrowStatement of throwStatement
		 | TryStatement of tryStatement

	type statementNoShortIf =
		  StatementWithoutTrailingSubstatement of statementWithoutTrailingSubstatement
		 | LabeledStatementNoShortIf of labeledStatementNoShortIf
		| IfThenElseStatementNoShortIf of ifThenElseStatementNoShortIf
		 | WhileStatementNoShortIf of whileStatementNoShortIf
		 | ForStatementNoShortIf of forStatementNoShortIf

	 type ifThenStatement = IfThenStatement of (expression * statement)
	type ifThenElseStatement = IfThenElseStatement of (expression * statementNoShortIf * statement)
	type ifThenElseStatementNoShortIf = IfThenElseStatementNoShortIf of (expression * statementNoShortIf * statementNoShortIf)

	type statement =
		 StatementWithoutTrailingSubstatement of statementWithoutTrailingSubstatement
		 | LabeledStatement of labeledStatement
		 | ifThenStatement
		| `IfThenElseStatement of ifThenElseStatement
		 | WhileStatement of whileStatement
		 | ForStatement of forStatement

	type localVariableDeclarationStatement = LocalVariableDeclarationStatement of localVariableDeclaration
	type localVariableDeclaration = LocalVariableDeclaration of (variableModifiers * jtype * variableDeclarators)

	type blockStatement =
		  localVariableDeclarationStatement
		| ClassOrInterfaceDeclaration of classOrInterfaceDeclaration
		| Statement of statement;;
	type blockStatements = BlockStatements of blockStatement list
	type block = Block of blockStatements
	*)

	type variableDeclaration = Integer of int
	type expression = Bool of bool

	type statement =
			VariableDeclaration of variableDeclaration
		| IfStatement of (expression * (block) * (block))
		| StatementBlock of block
	and block = Block of (statement list)


# 109 "blocksAST.ml"
let _eRR =
  Error

let rec _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_statement -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv110 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statementNoShortIf) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv108 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statementNoShortIf) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), expr), _, s1), _, s2) = _menhir_stack in
        let _v : 'tv_ifThenElseStatement = 
# 82 "blocksAST.mly"
                                                                                                ( IfStatement(expr, Block([s1]), Block([s2])) )
# 126 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv106) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenElseStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv104) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenElseStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv102) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_ifThenElseStatement) = _v in
        ((let _v : 'tv_statement = 
# 86 "blocksAST.mly"
                        ( s )
# 143 "blocksAST.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv103)) : 'freshtv105)) : 'freshtv107)) : 'freshtv109)) : 'freshtv111)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv120 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv118 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), expr), _, s) = _menhir_stack in
        let _v : 'tv_ifThenStatement = 
# 80 "blocksAST.mly"
                                                                    ( IfStatement(expr, Block([s]), Block([])) )
# 155 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv116) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv114) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv112) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_ifThenStatement) = _v in
        ((let _v : 'tv_statement = 
# 85 "blocksAST.mly"
                    ( s )
# 172 "blocksAST.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv113)) : 'freshtv115)) : 'freshtv117)) : 'freshtv119)) : 'freshtv121)
    | MenhirState21 | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv124 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv122 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : 'tv_blockStatement = 
# 99 "blocksAST.mly"
              ( s )
# 184 "blocksAST.ml"
         in
        _menhir_goto_blockStatement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv123)) : 'freshtv125)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv140 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv138 * _menhir_state * 'tv_statement) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv134 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv132 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, c) = _menhir_stack in
            let _v : (
# 75 "blocksAST.mly"
      (block)
# 205 "blocksAST.ml"
            ) = 
# 78 "blocksAST.mly"
                  ( Block([c]) )
# 209 "blocksAST.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv130) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 75 "blocksAST.mly"
      (block)
# 217 "blocksAST.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv128) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 75 "blocksAST.mly"
      (block)
# 225 "blocksAST.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv126) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_1 : (
# 75 "blocksAST.mly"
      (block)
# 233 "blocksAST.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv127)) : 'freshtv129)) : 'freshtv131)) : 'freshtv133)) : 'freshtv135)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv136 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv137)) : 'freshtv139)) : 'freshtv141)

and _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_statementWithoutTrailingSubstatement -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, s) = _menhir_stack in
    let _v : 'tv_statement = 
# 84 "blocksAST.mly"
                                         ( s )
# 250 "blocksAST.ml"
     in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_blockStatements : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_blockStatements -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state) * _menhir_state * 'tv_blockStatements) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | CLOSE_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv89 * _menhir_state) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, b) = _menhir_stack in
            let _v : 'tv_block = 
# 92 "blocksAST.mly"
                                               ( Block(b) )
# 277 "blocksAST.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv85) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv83) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv81) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (b : 'tv_block) = _v in
            ((let _v : 'tv_statementWithoutTrailingSubstatement = 
# 88 "blocksAST.mly"
          ( StatementBlock(b) )
# 294 "blocksAST.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv79) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_statementWithoutTrailingSubstatement) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState7 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = Obj.magic _menhir_stack in
                ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
                    let _v : 'tv_statementNoShortIf = 
# 90 "blocksAST.mly"
                                         ( s )
# 318 "blocksAST.ml"
                     in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv67) = _menhir_stack in
                    let (_menhir_s : _menhir_state) = _menhir_s in
                    let (_v : 'tv_statementNoShortIf) = _v in
                    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv65 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
                    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    let _tok = _menhir_env._menhir_token in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv63 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statementNoShortIf) = _menhir_stack in
                    let (_tok : token) = _tok in
                    ((match _tok with
                    | ELSE ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv59 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
                        ((let _tok = _menhir_discard _menhir_env in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv57 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statementNoShortIf) = _menhir_stack in
                        let (_tok : token) = _tok in
                        ((match _tok with
                        | IF ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                        | OPEN_BRACKET ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv58)) : 'freshtv60)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv61 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)) : 'freshtv64)) : 'freshtv66)) : 'freshtv68)) : 'freshtv70)
                | CLOSE_BRACKET | EOF | IF | INTEGER _ | OPEN_BRACKET ->
                    _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)) : 'freshtv76)
            | MenhirState0 | MenhirState21 | MenhirState1 | MenhirState10 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv77 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = Obj.magic _menhir_stack in
                (_menhir_reduce11 _menhir_env (Obj.magic _menhir_stack) : 'freshtv78)) : 'freshtv80)) : 'freshtv82)) : 'freshtv84)) : 'freshtv86)) : 'freshtv88)) : 'freshtv90)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv91 * _menhir_state) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)) : 'freshtv94)) : 'freshtv96)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * 'tv_blockStatement) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * 'tv_blockStatement) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, b), _, bs) = _menhir_stack in
        let _v : 'tv_blockStatements = 
# 95 "blocksAST.mly"
                                      ( b::bs )
# 385 "blocksAST.ml"
         in
        _menhir_goto_blockStatements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv98)) : 'freshtv100)
    | _ ->
        let (() : unit) = () in
        ((Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false) : 'freshtv101)

and _menhir_goto_blockStatement : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_blockStatement -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_blockStatement) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv53 * _menhir_state * 'tv_blockStatement) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | INTEGER _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | OPEN_BRACKET ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | CLOSE_BRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_blockStatement) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, b) = _menhir_stack in
        let _v : 'tv_blockStatements = 
# 94 "blocksAST.mly"
                   ( [b] )
# 417 "blocksAST.ml"
         in
        _menhir_goto_blockStatements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv52)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv54)) : 'freshtv56)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 71 "blocksAST.mly"
       (int)
# 428 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : (
# 71 "blocksAST.mly"
       (int)
# 438 "blocksAST.ml"
    )) = _v in
    ((let _v : 'tv_localVariableDeclarationStatement = 
# 103 "blocksAST.mly"
            ( VariableDeclaration(Integer(i)) )
# 443 "blocksAST.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv47) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_localVariableDeclarationStatement) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_localVariableDeclarationStatement) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (lvds : 'tv_localVariableDeclarationStatement) = _v in
    ((let _v : 'tv_blockStatement = 
# 97 "blocksAST.mly"
                                         ( lvds )
# 460 "blocksAST.ml"
     in
    _menhir_goto_blockStatement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv44)) : 'freshtv46)) : 'freshtv48)) : 'freshtv50)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_blockStatement) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state) * 'tv_expression) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state) * 'tv_expression) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv42)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv31 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INTEGER _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | OPEN_BRACKET ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv32)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | OPEN_PARENTHESIS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BOOLEAN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
            let (_v : (
# 72 "blocksAST.mly"
       (bool)
# 545 "blocksAST.ml"
            )) = _v in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
            let (b : (
# 72 "blocksAST.mly"
       (bool)
# 553 "blocksAST.ml"
            )) = _v in
            ((let _v : 'tv_expression = 
# 101 "blocksAST.mly"
            ( Bool(b) )
# 558 "blocksAST.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv15) = _menhir_stack in
            let (_v : 'tv_expression) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * 'tv_expression) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv11 * _menhir_state) * 'tv_expression) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | CLOSE_PARENTHESIS ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv7 * _menhir_state) * 'tv_expression) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv5 * _menhir_state) * 'tv_expression) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | IF ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                | OPEN_BRACKET ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv6)) : 'freshtv8)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv9 * _menhir_state) * 'tv_expression) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)) : 'freshtv12)) : 'freshtv14)) : 'freshtv16)) : 'freshtv18)) : 'freshtv20)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)) : 'freshtv24)) : 'freshtv26)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)) : 'freshtv30)

and formule : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 75 "blocksAST.mly"
      (block)
# 613 "blocksAST.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = max_int;
        }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | OPEN_BRACKET ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2)) : 'freshtv4))




