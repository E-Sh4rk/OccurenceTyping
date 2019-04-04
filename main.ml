
open Cduce
open Ast
open Types_additions
open Checker

let _ =
    let fn = ref "test.j" in
    if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;

    let program = Parsing.parse_defs_file !fn in
    let test_def ctx (name,parsed_expr) =
      let parsed_expr = substitute_var ":tmp:" parsed_expr ctx in
      Format.printf "%s: " name ;
      begin try
        Utils.print_type (typeof empty_env (parser_expr_to_expr empty_tenv parsed_expr)) ;
        substitute_var ":tmp:" (Let(name, parsed_expr, Var ":tmp:")) ctx
      with Ill_typed -> Format.printf "Ill typed!\n" ; ctx
      end 
    in
    ignore (List.fold_left test_def (Var ":tmp:") program)
