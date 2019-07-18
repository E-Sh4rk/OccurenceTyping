let f = fun ( (Int -> Int) & (Any ->Bool)) x -> magic
let g = fun ( (Int -> Int) & (Any ->Bool)) x -> magic

let test =
   fun (x : Any) ->
     if (f x, g x) is (Int, Bool) then 1 else 2
