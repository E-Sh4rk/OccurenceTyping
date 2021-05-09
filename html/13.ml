type X = X -> Empty -> Any
let z = fun (((Empty -> Any) -> Empty -> Any ) -> (Empty -> Any)) f ->
      let delta = fun ( X -> (Empty -> Any) ) x ->
         f ( fun (Empty -> Any) v -> ( x x v ))
       in delta delta    