%{ (* Emacs, use -*- tuareg -*- to open this file. *)

   open Ast

   (*let parsing_error pos msg =
     Printf.eprintf "%s:\n  %s\n" (Position.string_of_pos pos) msg;
     exit 1*)

   let var_or_primitive = function
     (*| Id "cos" -> Primitive Cos
     | Id "sin" -> Primitive Sin
     | Id "exp" -> Primitive Exp
     | Id "inv" -> Primitive Inv
     | Id "neg" -> Primitive Neg*)
     | x -> Var x

   let rec tuple = function
     | [] -> assert false
     | [x] -> x
     | x :: xs -> Pair (tuple xs, x)

   let tuple xs = tuple (List.rev xs)

%}

%token EOF
%token FUN LET IN FST SND
%token IF IS THEN ELSE
%token LPAREN RPAREN COLON EQUAL COMMA
%token ARROW AND OR NEG
%token ANY EMPTY BOOL CHAR (*FLOAT*) INT TRUE FALSE
%token TIMES (*PLUS MINUS*)
%token<string> ID
(*%token<float> LFLOAT*)
%token<int> LINT
%token<bool> LBOOL
%token<char> LCHAR
%token MAGIC

%type<Ast.parser_expr> term
%start<Ast.parser_expr> unique_term
%start<(Ast.varname * Ast.parser_expr) list> program

%right ARROW (*IN*)
%left OR
%left AND
(*%left PLUS*)
%left TIMES
%nonassoc NEG

%%

program: a=definition* EOF { a }
(*| error {
  parsing_error (Position.lex_join $startpos $endpos) "Syntax error."
}*)

unique_term: t=term EOF { t }

term:
  a=abstraction { a }
| d=definition IN t=term { Let (fst d, snd d, t) }
(*| lhs=term b=binop rhs=term { App (App (Primitive b, lhs), rhs) }*)
| t=simple_term { t }
| IF t=term IS ty=typ THEN t1=term ELSE t2=term { Ite (t,ty,t1,t2) }

simple_term:
  a=simple_term b=atomic_term { App (a, b) }
| FST a=atomic_term { Projection (Fst, a) }
| SND a=atomic_term { Projection (Snd, a) }
(*| m=MINUS t=atomic_term { App (Primitive Neg, t) }*)
| a=atomic_term { a }

atomic_term:
  x=identifier { var_or_primitive x }
| l=literal { Const l }
| LPAREN ts=separated_nonempty_list(COMMA, term) RPAREN { tuple ts }

literal:
(*f=LFLOAT { Float f }*)
  i=LINT   { Int i }
| c=LCHAR  { Char c }
| b=LBOOL  { Bool b }
| MAGIC    { Magic }

%inline abstraction: FUN vs=identifier+ COLON LPAREN ty=typ RPAREN ARROW t=term
{
  make_lambda_abstraction vs ty t
}

%inline definition: LET i=identifier EQUAL t=term
{
  (i, t)
}

(*%inline binop :
| PLUS  { Add }
| TIMES { Mul }*)

identifier: x=ID { x }

typ:
  x=type_constant { x }
| lhs=typ ARROW rhs=typ
  { Cduce.mk_arrow (Cduce.cons lhs) (Cduce.cons rhs) }
| lhs=typ TIMES rhs=typ { Cduce.mk_times (Cduce.cons lhs) (Cduce.cons rhs) }
| NEG t=typ { Cduce.neg t }
| lhs=typ AND rhs=typ { Cduce.cap lhs rhs }
| lhs=typ OR rhs=typ  { Cduce.cup lhs rhs }
| LPAREN t=typ RPAREN { t }

type_constant:
(*  FLOAT { TyFloat }*)
  INT { Cduce.int_typ }
| CHAR { Cduce.char_typ }
| BOOL { Cduce.bool_typ }
| TRUE { Cduce.true_typ }
| FALSE { Cduce.false_typ }
| EMPTY { Cduce.empty }
| ANY { Cduce.any }

(*%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}*)
