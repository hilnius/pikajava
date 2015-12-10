
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | SEMICOLON
  | OPEN_PARENTHESIS
  | OPEN_BRACKET
  | INTEGER of (
# 18 "blocksAST.mly"
       (int)
# 16 "blocksAST.ml"
)
  | IF
  | FOR
  | EOF
  | ELSE
  | COMMA
  | CLOSE_PARENTHESIS
  | CLOSE_BRACKET
  | BOOLEAN of (
# 19 "blocksAST.mly"
       (bool)
# 28 "blocksAST.ml"
)

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState68
  | MenhirState61
  | MenhirState54
  | MenhirState36
  | MenhirState33
  | MenhirState31
  | MenhirState26
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState15
  | MenhirState13
  | MenhirState11
  | MenhirState9
  | MenhirState6
  | MenhirState5
  | MenhirState2
  | MenhirState0

# 1 "blocksAST.mly"
  

	type variableDeclaration = Integer of int
	type expression = Bool of bool

	type statement =
			VariableDeclaration of variableDeclaration
		| IfStatement of (expression * block * block)
		| WhileStatement of (expression * block)
		| ForStatement of (statement * expression * expression * block)
		| BlockStatement of block
	and block = Block of (statement list)


# 73 "blocksAST.ml"

let rec _menhir_reduce26 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_statementWithoutTrailingSubstatement -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, s) = _menhir_stack in
    let _v : 'tv_statement = 
# 65 "blocksAST.mly"
                                         ( s )
# 81 "blocksAST.ml"
     in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_basicForStatement : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_basicForStatement -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv305) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_basicForStatement) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv303) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (s : 'tv_basicForStatement) = _v in
    ((let _v : 'tv_forStatement = 
# 40 "blocksAST.mly"
                      ( s )
# 98 "blocksAST.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_forStatement) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv299) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_forStatement) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv297) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (s : 'tv_forStatement) = _v in
    ((let _v : 'tv_statement = 
# 70 "blocksAST.mly"
                 ( s )
# 115 "blocksAST.ml"
     in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)) : 'freshtv302)) : 'freshtv304)) : 'freshtv306)

and _menhir_goto_statementNoShortIf : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_statementNoShortIf -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv263 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) * _menhir_state * 'tv_forUpdateOpt)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv261 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) * _menhir_state * 'tv_forUpdateOpt)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, fi), _, expr), _, update), _, s) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_basicForStatement = 
# 45 "blocksAST.mly"
                                                                                                                                       ( ForStatement(fi, expr, update, Block([s])) )
# 137 "blocksAST.ml"
         in
        _menhir_goto_basicForStatement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)) : 'freshtv264)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv269 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv265 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FOR ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | IF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | INTEGER _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | OPEN_BRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | WHILE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv266)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv267 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv279 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv277 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, expr), _, s1), _, s2) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_ifThenElseStatementNoShortIf = 
# 32 "blocksAST.mly"
                                                                                                         ( IfStatement(expr, Block([s1]), Block([s2])) )
# 186 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenElseStatementNoShortIf) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv273) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenElseStatementNoShortIf) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv271) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_ifThenElseStatementNoShortIf) = _v in
        ((let _v : 'tv_statementNoShortIf = 
# 76 "blocksAST.mly"
                                 ( s )
# 203 "blocksAST.ml"
         in
        _menhir_goto_statementNoShortIf _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)) : 'freshtv274)) : 'freshtv276)) : 'freshtv278)) : 'freshtv280)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv289 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv287 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, expr), _, s) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_whileStatementNoShortIf = 
# 37 "blocksAST.mly"
                                                                                ( WhileStatement(expr, Block([s])) )
# 218 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_whileStatementNoShortIf) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_whileStatementNoShortIf) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_whileStatementNoShortIf) = _v in
        ((let _v : 'tv_statementNoShortIf = 
# 77 "blocksAST.mly"
                            ( s )
# 235 "blocksAST.ml"
         in
        _menhir_goto_statementNoShortIf _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)) : 'freshtv284)) : 'freshtv286)) : 'freshtv288)) : 'freshtv290)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv295 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv291 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FOR ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | IF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | INTEGER _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | OPEN_BRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv293 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)) : 'freshtv296)
    | _ ->
        _menhir_fail ()

and _menhir_goto_blockStatements : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_blockStatements -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv255 * _menhir_state) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CLOSE_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv251 * _menhir_state) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv249 * _menhir_state) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, b) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_block = 
# 80 "blocksAST.mly"
                                               ( Block(b) )
# 296 "blocksAST.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (b : 'tv_block) = _v in
            ((let _v : 'tv_statementWithoutTrailingSubstatement = 
# 72 "blocksAST.mly"
          ( BlockStatement(b) )
# 313 "blocksAST.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_statementWithoutTrailingSubstatement) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState11 | MenhirState15 | MenhirState54 | MenhirState20 | MenhirState36 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
                    let _v : 'tv_statementNoShortIf = 
# 74 "blocksAST.mly"
                                         ( s )
# 334 "blocksAST.ml"
                     in
                    _menhir_goto_statementNoShortIf _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)
                | CLOSE_BRACKET | EOF | FOR | IF | INTEGER _ | OPEN_BRACKET | WHILE ->
                    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)) : 'freshtv238)
            | MenhirState0 | MenhirState5 | MenhirState68 | MenhirState6 | MenhirState61 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_statementWithoutTrailingSubstatement) = Obj.magic _menhir_stack in
                (_menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) : 'freshtv240)
            | _ ->
                _menhir_fail ()) : 'freshtv242)) : 'freshtv244)) : 'freshtv246)) : 'freshtv248)) : 'freshtv250)) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv253 * _menhir_state) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state * 'tv_blockStatement) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state * 'tv_blockStatement) * _menhir_state * 'tv_blockStatements) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, b), _, bs) = _menhir_stack in
        let _v : 'tv_blockStatements = 
# 83 "blocksAST.mly"
                                      ( b::bs )
# 368 "blocksAST.ml"
         in
        _menhir_goto_blockStatements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)) : 'freshtv260)
    | _ ->
        _menhir_fail ()

and _menhir_goto_forUpdateOpt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_forUpdateOpt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((((('freshtv231 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) * _menhir_state * 'tv_forUpdateOpt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLOSE_PARENTHESIS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv227 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) * _menhir_state * 'tv_forUpdateOpt) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | IF ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INTEGER _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | OPEN_BRACKET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | WHILE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv228)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv229 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) * _menhir_state * 'tv_forUpdateOpt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_statement -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv177 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) * _menhir_state * 'tv_forUpdateOpt)) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv175 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) * _menhir_state * 'tv_forUpdateOpt)) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, fi), _, expr), _, update), _, s) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_basicForStatement = 
# 43 "blocksAST.mly"
                                                                                                                              ( ForStatement(fi, expr, update, Block([s])) )
# 428 "blocksAST.ml"
         in
        _menhir_goto_basicForStatement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)) : 'freshtv178)
    | MenhirState61 | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv187 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf)) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv185 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf)) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, expr), _, s1), _, s2) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_ifThenElseStatement = 
# 30 "blocksAST.mly"
                                                                                                ( IfStatement(expr, Block([s1]), Block([s2])) )
# 444 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenElseStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenElseStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_ifThenElseStatement) = _v in
        ((let _v : 'tv_statement = 
# 68 "blocksAST.mly"
                        ( s )
# 461 "blocksAST.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)) : 'freshtv182)) : 'freshtv184)) : 'freshtv186)) : 'freshtv188)
    | MenhirState11 | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv197 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv195 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, expr), _, s) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_ifThenStatement = 
# 28 "blocksAST.mly"
                                                                    ( IfStatement(expr, Block([s]), Block([])) )
# 476 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifThenStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_ifThenStatement) = _v in
        ((let _v : 'tv_statement = 
# 67 "blocksAST.mly"
                    ( s )
# 493 "blocksAST.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)) : 'freshtv192)) : 'freshtv194)) : 'freshtv196)) : 'freshtv198)
    | MenhirState5 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv207 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv205 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, expr), _, s) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_whileStatement = 
# 35 "blocksAST.mly"
                                                                       ( WhileStatement(expr, Block([s])) )
# 508 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_whileStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_whileStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_whileStatement) = _v in
        ((let _v : 'tv_statement = 
# 69 "blocksAST.mly"
                   ( s )
# 525 "blocksAST.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)) : 'freshtv202)) : 'freshtv204)) : 'freshtv206)) : 'freshtv208)
    | MenhirState68 | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : 'tv_blockStatement = 
# 87 "blocksAST.mly"
              ( s )
# 537 "blocksAST.ml"
         in
        _menhir_goto_blockStatement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)) : 'freshtv212)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, c) = _menhir_stack in
            let _2 = () in
            let _v : (
# 22 "blocksAST.mly"
      (block)
# 556 "blocksAST.ml"
            ) = 
# 25 "blocksAST.mly"
                  ( Block([c]) )
# 560 "blocksAST.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 22 "blocksAST.mly"
      (block)
# 568 "blocksAST.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 22 "blocksAST.mly"
      (block)
# 576 "blocksAST.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_1 : (
# 22 "blocksAST.mly"
      (block)
# 584 "blocksAST.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv214)) : 'freshtv216)) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223 * _menhir_state * 'tv_statement) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
    | _ ->
        _menhir_fail ()

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OPEN_PARENTHESIS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEAN _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv172)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "blocksAST.mly"
       (int)
# 626 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ELSE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * (
# 18 "blocksAST.mly"
       (int)
# 638 "blocksAST.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, i) = _menhir_stack in
        let _v : 'tv_labeledStatementNoShortIf = 
# 96 "blocksAST.mly"
            ( VariableDeclaration(Integer(90)) )
# 644 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_labeledStatementNoShortIf) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_labeledStatementNoShortIf) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_labeledStatementNoShortIf) = _v in
        ((let _v : 'tv_statementNoShortIf = 
# 75 "blocksAST.mly"
                              ( s )
# 661 "blocksAST.ml"
         in
        _menhir_goto_statementNoShortIf _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)) : 'freshtv166)) : 'freshtv168)
    | CLOSE_BRACKET | EOF | FOR | IF | INTEGER _ | OPEN_BRACKET | WHILE ->
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 18 "blocksAST.mly"
       (int)
# 673 "blocksAST.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OPEN_PARENTHESIS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEAN _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv158)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)

and _menhir_goto_expressionOpt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expressionOpt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv155 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv151 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INTEGER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | CLOSE_PARENTHESIS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState33 in
            ((let _v : 'tv_forUpdateOpt = 
# 54 "blocksAST.mly"
               ( Bool(true) )
# 727 "blocksAST.ml"
             in
            _menhir_goto_forUpdateOpt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv152)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv153 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)) : 'freshtv156)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_blockStatement : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_blockStatement -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_blockStatement) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FOR ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INTEGER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | OPEN_BRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | CLOSE_BRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_blockStatement) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, b) = _menhir_stack in
        let _v : 'tv_blockStatements = 
# 82 "blocksAST.mly"
                   ( [b] )
# 772 "blocksAST.ml"
         in
        _menhir_goto_blockStatements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv146)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv148)

and _menhir_goto_forInit : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_forInit -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_forInit) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv141) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (f : 'tv_forInit) = _v in
    ((let _v : 'tv_forInitOpt = 
# 47 "blocksAST.mly"
              ( f )
# 793 "blocksAST.ml"
     in
    _menhir_goto_forInitOpt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv142)) : 'freshtv144)

and _menhir_goto_statementExpressionList : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_statementExpressionList -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_statementExpressionList) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_statementExpressionList) = _v in
        ((let _v : 'tv_forInit = 
# 56 "blocksAST.mly"
                            ( BlockStatement(Block(s)) )
# 812 "blocksAST.ml"
         in
        _menhir_goto_forInit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)) : 'freshtv126)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state * 'tv_statementExpression)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_statementExpressionList) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state * 'tv_statementExpression)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (sel : 'tv_statementExpressionList) = _v in
        ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_statementExpressionList = 
# 62 "blocksAST.mly"
                                                          ( s::sel )
# 829 "blocksAST.ml"
         in
        _menhir_goto_statementExpressionList _menhir_env _menhir_stack _menhir_s _v) : 'freshtv128)) : 'freshtv130)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_statementExpressionList) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_statementExpressionList) = _v in
        ((let _v : 'tv_forUpdate = 
# 59 "blocksAST.mly"
                            ( Bool(true) )
# 844 "blocksAST.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_forUpdate) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_forUpdate) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (f : 'tv_forUpdate) = _v in
        ((let _v : 'tv_forUpdateOpt = 
# 53 "blocksAST.mly"
               ( f )
# 861 "blocksAST.ml"
         in
        _menhir_goto_forUpdateOpt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv132)) : 'freshtv134)) : 'freshtv136)) : 'freshtv138)) : 'freshtv140)
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "blocksAST.mly"
       (int)
# 870 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "blocksAST.mly"
       (int)
# 880 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce23 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 18 "blocksAST.mly"
       (int)
# 890 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, i) = _menhir_stack in
    let _v : 'tv_labeledStatement = 
# 94 "blocksAST.mly"
            ( VariableDeclaration(Integer(90)) )
# 897 "blocksAST.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_labeledStatement) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_labeledStatement) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (s : 'tv_labeledStatement) = _v in
    ((let _v : 'tv_statement = 
# 66 "blocksAST.mly"
                     ( s )
# 914 "blocksAST.ml"
     in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv118)) : 'freshtv120)) : 'freshtv122)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "blocksAST.mly"
       (bool)
# 921 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (b : (
# 19 "blocksAST.mly"
       (bool)
# 931 "blocksAST.ml"
    )) = _v in
    ((let _v : 'tv_expression = 
# 89 "blocksAST.mly"
            ( Bool(b) )
# 936 "blocksAST.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv113) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_expression) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CLOSE_PARENTHESIS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv85 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FOR ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | IF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | INTEGER _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | OPEN_BRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv86)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv87 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)) : 'freshtv90)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CLOSE_PARENTHESIS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv91 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FOR ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | IF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | INTEGER _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | OPEN_BRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | WHILE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv92)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv93 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv101 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CLOSE_PARENTHESIS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv97 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FOR ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | IF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | INTEGER _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | OPEN_BRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | WHILE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv98)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv99 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)) : 'freshtv102)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CLOSE_PARENTHESIS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv103 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FOR ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | IF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | INTEGER _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | OPEN_BRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | WHILE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv105 * _menhir_state)) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, f) = _menhir_stack in
        let _v : 'tv_expressionOpt = 
# 50 "blocksAST.mly"
                ( f )
# 1085 "blocksAST.ml"
         in
        _menhir_goto_expressionOpt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)
    | _ ->
        _menhir_fail ()) : 'freshtv114)) : 'freshtv116)

and _menhir_goto_forInitOpt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_forInitOpt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv83 * _menhir_state)) * _menhir_state * 'tv_forInitOpt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv79 * _menhir_state)) * _menhir_state * 'tv_forInitOpt) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEAN _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv77) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState31 in
            ((let _v : 'tv_expressionOpt = 
# 51 "blocksAST.mly"
                ( Bool(true) )
# 1114 "blocksAST.ml"
             in
            _menhir_goto_expressionOpt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv80)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv81 * _menhir_state)) * _menhir_state * 'tv_forInitOpt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)) : 'freshtv84)

and _menhir_reduce25 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 18 "blocksAST.mly"
       (int)
# 1132 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, i) = _menhir_stack in
    let _v : 'tv_localVariableDeclarationStatement = 
# 91 "blocksAST.mly"
            ( VariableDeclaration(Integer(90)) )
# 1139 "blocksAST.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_localVariableDeclarationStatement) = _v in
    ((match _menhir_s with
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_localVariableDeclarationStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_localVariableDeclarationStatement) = _v in
        ((let _v : 'tv_forInit = 
# 57 "blocksAST.mly"
                                      ( s )
# 1158 "blocksAST.ml"
         in
        _menhir_goto_forInit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv68)) : 'freshtv70)
    | MenhirState68 | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_localVariableDeclarationStatement) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (lvds : 'tv_localVariableDeclarationStatement) = _v in
        ((let _v : 'tv_blockStatement = 
# 85 "blocksAST.mly"
                                         ( lvds )
# 1173 "blocksAST.ml"
         in
        _menhir_goto_blockStatement _menhir_env _menhir_stack _menhir_s _v) : 'freshtv72)) : 'freshtv74)
    | _ ->
        _menhir_fail ()) : 'freshtv76)

and _menhir_reduce32 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 18 "blocksAST.mly"
       (int)
# 1182 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, i) = _menhir_stack in
    let _v : 'tv_statementExpression = 
# 98 "blocksAST.mly"
            ( VariableDeclaration(Integer(90)) )
# 1189 "blocksAST.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv65) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_statementExpression) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_statementExpression) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_statementExpression) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INTEGER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv58)
    | CLOSE_PARENTHESIS | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_statementExpression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : 'tv_statementExpressionList = 
# 61 "blocksAST.mly"
                        ( [s] )
# 1220 "blocksAST.ml"
         in
        _menhir_goto_statementExpressionList _menhir_env _menhir_stack _menhir_s _v) : 'freshtv60)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_statementExpression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)) : 'freshtv64)) : 'freshtv66)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_blockStatement) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv23 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv25 * _menhir_state)) * _menhir_state * 'tv_expression)) * _menhir_state * 'tv_statementNoShortIf)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv27 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) * _menhir_state * 'tv_forUpdateOpt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv29 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) * _menhir_state * 'tv_expressionOpt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv31 * _menhir_state)) * _menhir_state * 'tv_forInitOpt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * 'tv_statementExpression)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv37 * _menhir_state)) * _menhir_state * 'tv_expression)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv41 * _menhir_state)) * _menhir_state * 'tv_expression)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv45 * _menhir_state)) * _menhir_state * 'tv_expression)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv51 * _menhir_state)) * _menhir_state * 'tv_expression)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv56)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OPEN_PARENTHESIS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEAN _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FOR ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | INTEGER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | OPEN_BRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "blocksAST.mly"
       (int)
# 1374 "blocksAST.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OPEN_PARENTHESIS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEAN _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv14)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OPEN_PARENTHESIS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INTEGER _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState22 in
            let (_v : (
# 18 "blocksAST.mly"
       (int)
# 1426 "blocksAST.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
            | SEMICOLON ->
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv3 * _menhir_state * (
# 18 "blocksAST.mly"
       (int)
# 1443 "blocksAST.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)) : 'freshtv6)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState22 in
            ((let _v : 'tv_forInitOpt = 
# 48 "blocksAST.mly"
              ( VariableDeclaration(Integer(9)) )
# 1454 "blocksAST.ml"
             in
            _menhir_goto_forInitOpt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and formule : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 22 "blocksAST.mly"
      (block)
# 1484 "blocksAST.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FOR ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INTEGER _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | OPEN_BRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 99 "blocksAST.mly"
  

# 1521 "blocksAST.ml"

# 220 "/home/hilnius/.opam/4.02.1/lib/menhir/standard.mly"
  


# 1527 "blocksAST.ml"
