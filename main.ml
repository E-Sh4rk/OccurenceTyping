
open Cduce
open Types_additions
open Ast

let alpha = mk_var false "a"
let beta  = mk_var false "b"

let int_t = mk_atom "int"
let string_t = mk_atom "string"

let bool_to_int =
    let var = "x" in
    let expr = Const Magic in
    let typ = mk_arrow (cons bool_typ) (cons int_typ) in
    Lambda (typ, var, expr)

let int_incr =
    let var = "x" in
    let expr = Const Magic in
    let typ = mk_arrow (cons int_typ) (cons int_typ) in
    Lambda (typ, var, expr)

let wt_ast =
    let var = "x" in
    let expr = Ite (Var var, int_typ, App(Var "incr", Var var), App(Var "bti", Var var)) in
    let expr = Let ("incr", int_incr, expr) in
    let expr = Let ("bti", bool_to_int, expr) in
    let in_typ = cup int_typ bool_typ in
    let out_typ = int_typ in
    let typ = mk_arrow (cons in_typ) (cons out_typ) in
    Lambda (typ, var, expr)

let it_ast =
    let var = "x" in
    let expr = Ite (Var var, any, App(Var "incr", Var var), App(Var "bti", Var var)) in
    let expr = Let ("incr", int_incr, expr) in
    let expr = Let ("bti", bool_to_int, expr) in
    let in_typ = cup int_typ bool_typ in
    let out_typ = int_typ in
    let typ = mk_arrow (cons in_typ) (cons out_typ) in
    Lambda (typ, var, expr)

let _ =
    (* Test records and recursive types *)
    let alpha_list = mk_list (cons alpha) in
    let beta_list = mk_list (cons beta) in
    let beta_list_list = mk_list beta_list in
    let union = cup (descr alpha_list) (descr beta_list_list) in
    let record = mk_record true [("al",alpha_list);("bl",beta_list);("al_bll_union", cons union)] in
    Utils.print_type record ;
    let record' = mk_record true [("al",beta_list)] in
    let inter = cap record record' in
    Utils.print_type inter ;
    let proj = get_field inter "al" in
    Utils.print_type proj ;
    (* Test functions *)
    let f1 = mk_arrow (cons int_t) (cons string_t) in
    let f2 = mk_arrow (cons string_t) (cons int_t) in
    let f = cap f1 f2 in
    Utils.print_type f ;
    Utils.print_type (domain f) ;
    Utils.print_type (apply f int_t) ;
    (* Test custom operators *)
    Utils.print_type (square f int_t) ;
    (* Occurence typing *)
    Utils.print_type (typeof empty_env wt_ast) ;
    Utils.print_type (typeof empty_env it_ast)
