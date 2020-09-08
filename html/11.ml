
type X = X -> Any -> Empty

(* Fix-point combinator *)
let z = fun (((Any -> Empty) -> Any -> Empty ) -> (Any -> Empty)) f ->
      let delta = fun ( X -> (Any -> Empty) ) x ->
         f ( fun (Any -> Empty) v -> ( x x v ))
       in delta delta

let id = fun ((Any -> Empty) -> (Any -> Empty)) x -> x

let diverge = z id
