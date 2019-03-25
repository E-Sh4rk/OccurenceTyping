
open Cduce
open Types_additions

let alpha = mk_var false "a"
let beta  = mk_var false "b"

let int_t = mk_atom "int"
let string_t = mk_atom "string"

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
    Utils.print_type (square f int_t)
