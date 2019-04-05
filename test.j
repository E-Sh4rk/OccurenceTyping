(* Basic functions *)

let incr =
    fun i : (Int -> Int) -> magic

let plus =
    fun i : (Int -> Int -> Int) -> magic

let lnot =
    fun x : ((True -> False) & (False -> True)) -> magic

let land =
    fun x : (Bool -> Bool -> Bool) -> magic

(* Basic example (first example of the paper) *)

let basic_ok =

    let bti =
        fun b : (Bool -> Int) -> magic
    in

    fun x : ((Int | Bool) -> Int) ->
        if x is Int then incr x else bti x

let basic_fail =

    let bti =
        fun b : (Bool -> Int) -> magic
    in

    fun x : ((Int | Bool) -> Int) ->
        if x is Any then incr x else bti x

(* Example with Let and Pairs *)

let let_pairs_ok =

    fun x : (((Int * Int) -> Int) & ((Bool * Bool) -> Bool)) ->
        let y = fst x in let z = snd x in
        if x is (Int * Any) then plus y z else land (lnot y) z

(* Example that shows that backtyping is stronger if we intersect with the environment as soon as possible *)
(* Also need the improved version of the square operator *)

let two_steps =

        let f = fun x : (( (Any \ Int) -> ((Any * Any) \ (Int * Int)) ) & (Int -> (Int * Int))) -> magic
        in
        fun x : (Any -> Int) ->
                if snd (f x) is Int
                then
                        if fst (f x) is Int then x
                        else 0
                else 0

(* Example with recursive types and lists *)

atom nil
type X = Nil | (Any * X)
