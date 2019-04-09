
open Cduce
open Ast
open Types_additions
open Checker
open IO

let _ =
    let fn = ref "test.j" in
    if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;

    let program = parse_program_file !fn in
    let test_def (tenv,idm,env) (name,parsed_expr) =
      Format.printf "%s: " name ;
      begin try
        let id = unique_varid () in
        let annot_expr = parser_expr_to_annot_expr tenv idm parsed_expr in
        let typ = typeof env (annot_expr_to_expr annot_expr) in
        let idm = StrMap.add name id idm in
        let env = ExprMap.add (Var ((), id)) typ env in
        Utils.print_type typ ; (idm, env)
      with Ill_typed str -> Format.printf "Ill typed: %s\n" str ; (idm,env)
      end 
    in
    let treat_elem (tenv,idm,env) elem =
      match elem with
      | Definition d ->
        let (idm,env) = test_def (tenv,idm,env) d in
        (tenv,idm,env)
      | Atoms lst ->
        let tenv = List.fold_left define_atom tenv lst in
        (tenv,idm,env)
      | Types lst ->
        let tenv = define_types tenv lst in
        (tenv,idm,env)
    in
    ignore (List.fold_left treat_elem (empty_tenv, empty_id_map, empty_env) program)
