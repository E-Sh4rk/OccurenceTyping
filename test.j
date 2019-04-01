
let bti =
    fun b : (Bool -> Int) -> magic
in 

let incr =
    fun i : (Int -> Int) -> magic
in

fun x : ((Int | Bool) -> Int) ->
    if x is Int then incr x else bti x

