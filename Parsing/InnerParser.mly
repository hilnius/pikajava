%{
    open AST
    open Type
%}

(**************
 * The tokens *
 **************)
	  
  (* Operators *)
%token OP_MUL OP_DIV OP_MOD OP_ADD OP_SUB
%token OP_AND OP_OR OP_XOR //OP_NOT
%token OP_CAND OP_COR OP_COND
%token OP_GT OP_LT OP_GE OP_LE OP_EQ OP_NE
%token OP_SHL OP_SHR OP_SHRR
//%token OP_BNOT
%token OP_INC OP_DEC

(* Assignment Operators *)
%token ASSIGN
%token ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB
%token ASS_SHL ASS_SHR ASS_SHRR ASS_AND ASS_XOR ASS_OR

(* Separators *)
%token LPAREN RPAREN 

(* Literal values *)
%token <string> INT_LIT
%token <string> FLOAT_LIT
%token <string> STRING
%token <char> CHAR_LIT


(********************************
 * Priorities and associativity *
 ********************************)
%left OP_OR OP_COR
%left OP_XOR
%left OP_AND OP_CAND
%left OP_EQ OP_NE
%left OP_GT OP_LT OP_GE OP_LE INSTANCEOF
%left OP_SHL OP_SHR OP_SHRR
%left OP_ADD OP_SUB
%left OP_MUL OP_DIV OP_MOD
%right OP_COND COLON
%right ASSIGN ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB ASS_SHL ASS_SHR ASS_SHRR ASS_AND ASS_XOR ASS_OR
%right CAST
%left OP_INC OP_DEC       
%left DOT

%type <AST.statement list> block

%%
(*************
 * The rules *
 *************)

%public block:
  |  bsl=body(blockStatement* ) { bsl }

blockStatement:
  | variableModifier t=aType vdl=separated_nonempty_list(COMMA,variableDeclarator) SEMI {
	VarDecl (List.map (fun (id,n,init) -> Type.mk_array n t, id, init) vdl)
    }
  | b=block {
	Block b
    }
  | SEMI {
	Nop
    }
  | WHILE LPAREN e=expression RPAREN b=blockStatement {
	While (e,b)
    }
  | FOR LPAREN i=forInit SEMI e=expression? SEMI u=separated_list(COMMA,expression) RPAREN b=blockStatement {
	For (i,e,u,b)
    }
  | IF LPAREN e=expression RPAREN b=blockStatement {
	If (e,b,None)
    }
  | IF LPAREN e=expression RPAREN b1=blockStatement ELSE b2=blockStatement {
	If (e,b1,Some b2)
    }
  | RETURN e=expression? SEMI {
	Return (e)
    }
  | THROW e=expression SEMI {
	Throw (e)
    }
  | TRY b1=block c=catch* FINALLY b2=block {
	Try (b1,c,b2)
    }
  | TRY b1=block c=catch+ {
	Try (b1,c,[])
    }
  | e=expression1 SEMI {
	match e with
	| `Exp e -> Expr e
	| `Decl e -> e
    }
  | p = primitiveType l = list(pair(LBRACKET,RBRACKET)) vdl=separated_nonempty_list(COMMA,variableDeclarator) SEMI {
	let t = if List.length l > 0 then Array(Primitive p,List.length l) else Primitive p in
	VarDecl (List.map (fun (id,n,init) -> Type.mk_array n t, id, init) vdl)
    }

expression1:
  | LPAREN e=expression RPAREN { `Exp e }
  | e1=expression op=assign_op e2=expression { `Exp { edesc = AssignExp(e1,op,e2) } }
  | e1=expression OP_COND e2=expression COLON e3=expression { `Exp { edesc = CondOp(e1,e2,e3) } }
  | e1=expression op=infix_op e2=expression { `Exp { edesc = Op(e1,op,e2) } }
  | e1=expression INSTANCEOF e2=expression { `Exp { edesc = Instanceof(e1,e2) } }
  | LPAREN e1=expression RPAREN e2=expression { `Exp { edesc = Cast(e1,e2) } }
  | m=name LPAREN params=separated_list(COMMA,expression) RPAREN { `Exp { edesc = Call({ edesc = Name("this") },m,params) } }
  | o=expression DOT m=IDENTIFIER LPAREN params=separated_list(COMMA,expression) RPAREN { `Exp { edesc = Call(o,m,params) } }
  | o=expression DOT n=name  { `Exp { edesc = Attr(o,n) } }
  | NEW id=qualifiedName LPAREN params=separated_list(COMMA,expression) RPAREN  { `Exp { edesc = New(id,params) } }
  | l=literal { `Exp { edesc = Val(l) } }
  | id=name { `Exp { edesc = Name(id) } }
  | l0=separated_nonempty_list(DOT,IDENTIFIER) l = list(pair(LBRACKET,RBRACKET)) vdl=separated_nonempty_list(COMMA,variableDeclarator) {
        let t = Type.mk_type l0 in	
        let t = if List.length l > 0 then Array(Ref t,List.length l) else Ref t in
	`Decl (VarDecl (List.map (fun (id,n,init) -> Type.mk_array n t, id, init) vdl))
    }

%inline catch:
  | CATCH LPAREN p=formalParameter RPAREN b=block { p,b }

forInit:
  | { [] }
  | variableModifier? t=aType vdl=separated_nonempty_list(COMMA,variableDeclarator) { List.map (fun (id,n,init) -> Type.mk_array n t, id, init) vdl }

%inline variableModifier: FINAL { }

	      
%public %inline variableDeclarator:
  | id=IDENTIFIER l=list(pair(LBRACKET,RBRACKET)) init=option(preceded(ASSIGN,variableInitializer)) { id, List.length l, init }

variableInitializer:
  | e = expression { e }
  | e = arrayInitializer { e }

arrayInitializer:
  | l=body(separated_list(COMMA,variableInitializer)) { { edesc = ArrayInit l } }


expression:
  | LPAREN e=expression RPAREN { e }
  | e=expression op=postfix_op { { edesc = Post(e,op) } }
  | e1=expression op=assign_op e2=expression { { edesc = AssignExp(e1,op,e2) } }
  | e1=expression OP_COND e2=expression COLON e3=expression { { edesc = CondOp(e1,e2,e3) } }
  | e1=expression op=infix_op e2=expression { { edesc = Op(e1,op,e2) } }
  | e1=expression INSTANCEOF e2=expression { { edesc = Instanceof(e1,e2) } }
  | LPAREN e1=expression RPAREN e2=expression %prec CAST { { edesc = Cast(e1,e2) } }
  | m=name LPAREN params=separated_list(COMMA,expression) RPAREN { { edesc = Call({ edesc = Name("this") },m,params) } }
  | o=expression DOT m=IDENTIFIER LPAREN params=separated_list(COMMA,expression) RPAREN { { edesc = Call(o,m,params) } }
  | o=expression DOT n=name  { { edesc = Attr(o,n) } }
  | NEW id=qualifiedName LPAREN params=separated_list(COMMA,expression) RPAREN  { { edesc = New(id,params) } }
  | l=literal { { edesc = Val(l) } }
  | id=name { { edesc = Name(id) } }

%inline qualifiedName: l = separated_nonempty_list(DOT, name) { l }

%inline postfix_op:
  | OP_INC { Incr }
  | OP_DEC { Decr }
    
%inline name:
  | THIS { "this" }
  | SUPER { "super" }
  | id=IDENTIFIER { id }
						  
%inline assign_op:
  | ASSIGN   { Assign   }
  | ASS_ADD  { Ass_add  }
  | ASS_SUB  { Ass_sub  }
  | ASS_MUL  { Ass_mul  }
  | ASS_DIV  { Ass_div  }
  | ASS_MOD  { Ass_mod  }
  | ASS_SHL  { Ass_shl  }
  | ASS_SHR  { Ass_shr  }
  | ASS_SHRR { Ass_shrr }
  | ASS_AND  { Ass_and  }
  | ASS_XOR  { Ass_xor  }
  | ASS_OR   { Ass_or   }

%inline infix_op:
  | OP_COR  { Op_cor  }
  | OP_CAND { Op_cand }
  | OP_OR   { Op_or   }
  | OP_AND  { Op_and  }
  | OP_XOR  { Op_xor  }
  | OP_EQ   { Op_eq   }
  | OP_NE   { Op_ne   }
  | OP_GT   { Op_gt   }
  | OP_LT   { Op_lt   }
  | OP_GE   { Op_ge   }
  | OP_LE   { Op_le   }
  | OP_SHL  { Op_shl  }
  | OP_SHR  { Op_shr  }
  | OP_SHRR { Op_shrr }
  | OP_ADD  { Op_add  }
  | OP_SUB  { Op_sub  }
  | OP_MUL  { Op_mul  }
  | OP_DIV  { Op_div  }
  | OP_MOD  { Op_mod  }

%inline literal:
  | i = INT_LIT { AST.Int i }
  | f = FLOAT_LIT { AST.Float f }
  | c = CHAR_LIT { AST.Char c }
  | s = STRING { String s }
  | TRUE { AST.Boolean true}
  | FALSE { AST.Boolean false }
  | NULL { Null }



