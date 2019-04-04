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
%token FUN LET IN FST SND DEBUG
%token IF IS THEN ELSE
%token LPAREN RPAREN COLON EQUAL COMMA
%token ARROW AND OR NEG DIFF
%token ANY EMPTY BOOL CHAR (*FLOAT*) INT TRUE FALSE UNIT
%token TIMES (*PLUS MINUS*)
%token ATOMS TYPE TYPE_AND
%token<string> ID
%token<string> TID
(*%token<float> LFLOAT*)
%token<int> LINT
%token<bool> LBOOL
%token<char> LCHAR
%token<string> LSTRING
%token LUNIT
%token MAGIC

%type<Ast.parser_expr> term
%start<Ast.parser_expr> unique_term
%start<(Ast.varname * Ast.parser_expr) list> definitions
%start<Ast.parser_program> program

%right ARROW (*IN*)
%left OR
%left AND
(*%left PLUS*)
%left TIMES
%nonassoc DIFF
%nonassoc NEG

%%

program: e=element* EOF { e }

definitions: a=definition* EOF { a }
(*| error {
  parsing_error (Position.lex_join $startpos $endpos) "Syntax error."
}*)

unique_term: t=term EOF { t }


element:
  a=definition { Definition a }
| a=atoms      { Atoms a }
| a=types_def  { Types a }

atoms: ATOMS a=ID* { a }

types_def: TYPE ts=separated_nonempty_list(TYPE_AND, name_and_typ) { ts }

name_and_typ: name=TID EQUAL t=typ { (name, t) }

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
| DEBUG str=LSTRING a=atomic_term { Debug (str, a) }
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
| LUNIT    { Unit }
| MAGIC    { Magic }

%inline abstraction: FUN vs=identifier+ COLON LPAREN ty=typ RPAREN ARROW t=term
{
  if List.length vs > 1 then failwith "Fun with multiple arguments not supported yet!"
  else Lambda (ty, List.hd vs, t)
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
  x=type_constant { TBase x }
| s=TID { TCustom s }
| lhs=typ ARROW rhs=typ { TArrow (lhs, rhs) }
| lhs=typ TIMES rhs=typ { TPair (lhs, rhs) }
| NEG t=typ { TNeg t }
| lhs=typ AND rhs=typ { TCap (lhs, rhs) }
| lhs=typ OR rhs=typ  { TCup (lhs, rhs) }
| lhs=typ DIFF rhs=typ  { TDiff (lhs, rhs) }
| LPAREN t=typ RPAREN { t }

type_constant:
(*  FLOAT { TyFloat }*)
  INT { TInt }
| CHAR { TChar }
| BOOL { TBool }
| TRUE { TTrue }
| FALSE { TFalse }
| UNIT { TUnit }
| EMPTY { TEmpty }
| ANY { TAny }

(*%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}*)
