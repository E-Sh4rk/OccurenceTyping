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
    let expr' = match expr with
    | Const c -> Const c
    | Var v  -> Var v
    | Lambda (t, v, e) -> Lambda (t, v, e)
    | RecLambda (s, t, v, e) -> RecLambda (s, t, v, e)
    | InfLambda (t, v, e) -> InfLambda (t, v, e)
    | Ite (e, t, e1, e2) ->
      let abstract_var (annot, expr) v =
        let t = VarIdMap.find v varenv in
        let newvar = unique_varid () in
        let new_ite = substitute_var v (annot, Var newvar) (annot, expr) in
        let lambda_type = (* Cduce.mk_arrow (Cduce.cons *) t(* ) Cduce.any_node *) in
        (* TODO : DEBUG *)
        let abs = (annot, InfLambda (lambda_type, newvar, new_ite)) in
        (annot, App (abs, (annot, Var v)))
      in
      let free_vars = (List.of_seq (VarIdSet.to_seq (fv (annot, expr)))) in
      let (_, ret) = List.fold_left abstract_var (annot, expr) free_vars
      in ret
    | App (e1, e2) -> App (aux e1, aux e2)
    | Let (v, e1, e2) -> assert false
    | Pair (e1, e2) -> Pair (aux e1, aux e2)
    | Projection (p, e) -> Projection (p, aux e)
    | RecordUpdate (e1, l, e2) ->
        RecordUpdate (aux e1, l, Utils.option_map aux e2)
    | Debug (str, e) -> Debug (str, aux e)
    in
    (annot, expr')
  in
  aux aexpr
