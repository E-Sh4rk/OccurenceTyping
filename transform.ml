open Ast

let rec substitute_var v ve (a,e) =
    let e = match e with
    | Const c -> Const c
    | Var v' when v=v' -> snd ve
    | Var v' -> Var v'
    | Lambda (t, v', e) when v=v' -> Lambda (t, v', e)
    | Lambda (t, v', e) -> Lambda (t, v', substitute_var v ve e)
    | RecLambda (s, t, v', e) when v=v' || v=s -> RecLambda (s, t, v', e)
    | RecLambda (s, t, v', e) -> RecLambda (s, t, v', substitute_var v ve e)
    | InfLambda (t, v', e) when v=v' -> InfLambda (t, v', e)
    | InfLambda (t, v', e) -> InfLambda (t, v', substitute_var v ve e)
    | Ite (e, t, e1, e2) -> Ite (substitute_var v ve e, t, substitute_var v ve e1, substitute_var v ve e2)
    | App (e1, e2) -> App (substitute_var v ve e1, substitute_var v ve e2)
    | Let (v', e1, e2) when v=v' -> Let (v', substitute_var v ve e1, e2)
    | Let (v', e1, e2) -> Let (v', substitute_var v ve e1, substitute_var v ve e2)
    | Pair (e1, e2) -> Pair (substitute_var v ve e1, substitute_var v ve e2)
    | Projection (p, e) -> Projection (p, substitute_var v ve e)
    | RecordUpdate (e1, l, e2) ->
        RecordUpdate (substitute_var v ve e1, l, Utils.option_map (substitute_var v ve) e2)
    | Debug (str, e) -> Debug (str, substitute_var v ve e)
    in
    (a,e)

let abstract_unabstracted_ite aexpr (varenv:typ VarIdMap.t) =
  let rec aux (annot, expr) =
    let annot = copy_annot annot in
    match expr with
    | Const _ | Var _ | Lambda _ | RecLambda _ | InfLambda _ -> (annot, expr)
    | Ite _ ->
      let abstract_var (annot, expr) v =
        let annot1 = copy_annot annot in
        let annot2 = copy_annot annot in
        let t = VarIdMap.find v varenv in
        let newvar = unique_varid () in
        let new_ite = substitute_var v (annot1, Var newvar) (annot, expr) in
        let lambda_type = t in
        (annot2, InfLambda (lambda_type, newvar, new_ite))
      in
      let free_vars = (List.of_seq (VarIdSet.to_seq (fv (annot, expr)))) in
      let abs = List.fold_left abstract_var (annot, expr) free_vars in
      let app_var (annot, expr) v =
        let annot1 = copy_annot annot in
        let annot2 = copy_annot annot in
        (annot2, App ((annot, expr), (annot1, Var v)))
      in
      List.fold_left app_var abs (List.rev free_vars)
    | App (e1, e2) -> (annot, App (aux e1, aux e2))
    | Let (v, e1, e2) -> assert false
    | Pair (e1, e2) -> (annot, Pair (aux e1, aux e2))
    | Projection (p, e) -> (annot, Projection (p, aux e))
    | RecordUpdate (e1, l, e2) ->
        (annot, RecordUpdate (aux e1, l, Utils.option_map aux e2))
    | Debug (str, e) -> (annot, Debug (str, aux e))
  in
  aux aexpr
