open Cduce
open Ast
open Types_additions
open Checker

let type_check_program
  (program:Ast.parser_program) (pr:string -> unit) pr_logs pr_ill_typed =
  let test_def (tenv,idm,env) (name,parsed_expr) =
    Format.ksprintf pr "%s: " name;
    begin try
        let id = unique_varid () in
        let annot_expr = parser_expr_to_annot_expr tenv idm parsed_expr in
        let time = Unix.gettimeofday () in 
        let typ = typeof env annot_expr in
        let time = (Unix.gettimeofday ()) -. time in
        let idm = StrMap.add name id idm in
        let env = ExprMap.add ((), Var id) typ env in
        Format.ksprintf pr "%s (checked in %fs)\n\n"
        (Cduce.string_of_type typ) time;
        pr_logs () ; (idm, env)
      with Ill_typed (pos, str) ->
        pr_ill_typed (pos, str); (idm,env)
      end
    in
    let treat_elem (tenv,idm,env) elem =
      match elem with
      | Definition d ->
        let (idm,env) = test_def (tenv,idm,env) d in
        (tenv,idm,env)
      | Atoms lst ->
        let tenv = List.fold_left define_atom tenv lst in
        let env = add_atoms_to_env env lst tenv in
        (tenv,idm,env)
      | Types lst ->
        let tenv = define_types tenv lst in
        (tenv,idm,env)
    in
    ignore (List.fold_left treat_elem (empty_tenv, empty_id_map, empty_env) program)
