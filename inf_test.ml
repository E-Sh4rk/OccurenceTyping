(* Basic functions *)
let incr =
    fun (Int -> Int) i -> magic

let plus =
    fun (Int -> Int -> Int) i -> magic

let lnot =
    fun ((True -> False) & (False -> True)) x -> magic

let land =
    fun (Bool -> Bool -> Bool) x -> magic

let bti =
  fun (Bool -> Int) b -> magic

(* Basic example (first example of the paper) *)

let basic_ok =
    fun ((Int | Bool) -> Int) x ->
        if x is Int then incr x else bti x

let basic_inf =
  fun (y : Int | Bool) ->
  if y is Int then incr y else lnot y

let any_inf =
  fun (x : Any) ->
  if x is Int then incr x else
  if x is Bool then lnot x else
    x

atom nil
type IntBoolList = Nil | (Int|Bool, IntBoolList)

let map =
  fun (loop : ((Int -> Int)&(Bool -> Bool)) -> IntBoolList -> IntBoolList) f ->
  fun (l : IntBoolList) ->
  if l is Nil then l
  else let h = fst l in
    let t = snd l in
    if h is Int then (basic_inf h, loop f l)
    else loop f l
