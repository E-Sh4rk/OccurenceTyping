
open Cduce
open Types_additions
open Ast

let alpha = mk_var false "a"
let beta  = mk_var false "b"

let int_t = mk_atom "int"
let string_t = mk_atom "string"

let test_cduce _ =
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

let _ =    
    (* Occurence typing *)
    let fn = ref "test.j" in
    if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;

    let program = Parsing.parse_program_file !fn in
    let test_def ctx (name,parsed_expr) =
      let parsed_expr = substitute_var ":tmp:" parsed_expr ctx in
      Format.printf "%s: " name ;
      begin try
        Utils.print_type (typeof empty_env (parser_expr_to_expr parsed_expr)) ;
        substitute_var ":tmp:" (Let(name, parsed_expr, Var ":tmp:")) ctx
      with Ill_typed -> Format.printf "Ill typed!\n" ; ctx
      end 
    in
    ignore (List.fold_left test_def (Var ":tmp:") program)
