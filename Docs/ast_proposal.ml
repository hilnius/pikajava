
type value =
| Float of float
| Int of int
| Object of object

type dictionary = map<string, value>; (* variables *)

type object = ObjectImpl of (className * objectInstance)

type context = Context of dictionary

type block = Block of statement list

type statement =

let call_statements = statements -> context -> context;;
let eval_statement = statement -> context -> context;;
let eval_expr = expression -> context -> (context * bool);;

let eval_if ast context =
	match ast with
	| IfStatement(ifCond, thenBlock, elseBlock) ->
		let (c1, result) = eval_expr(ifCond, context) in
			if (eval_expr exp) then
				call_statements thenBlock c1
			else
				call_statements elseBlock c1
;;

let rec call_statements statements context =
	match statements with
	| [] -> context
	| h::t -> call_statements t (eval_statement h context);

let eval_statement statement context =
	match statement with
	| IfStatement(_,_,_) -> eval_if statement context
