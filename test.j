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

(* Example with intervals *)

let interval =

        fun x : ([0;5] -> [-1;-1] | [1;1]) ->
                if x is [0;2] then -1
                else if x is [3;5] then 1
                else false

(* Example with Let and Pairs *)

let let_pairs_ok =

    fun x : (((Int * Int) -> Int) & ((Bool * Bool) -> Bool)) ->
        let y = fst x in let z = snd x in
        if x is (Int * Any) then plus y z else land (lnot y) z

(* Example that shows that backtyping is stronger if we intersect with the environment as soon as possible *)
(* Also need the improved version of the square operator *)

let two_steps =

        let f = fun x : (( Any\Int -> (Any * Any)\(Int * Int) ) & ( Int -> Int*Int )) -> magic
        in
        fun x : (Any -> Int) ->
                if snd (f x) is Int
                then
                        if fst (f x) is Int then x
                        else 0
                else 0

(* Example with recursive types and lists *)

atom nil
type List = Nil | (Any * List)
type IntList = Nil | (Int * IntList)
type BoolList = Nil | (Bool * BoolList)

let lists =

        fun lst : (BoolList|IntList -> IntList) ->
                if lst is Nil
                then nil
                else if fst lst is Int
                then lst
                else nil

type BIUList = Nil | (Bool * IUList)
and  IUList  = Nil | (Int * UList)
and  UList   = Nil | (Unit * BIUList)

let regex_next =

        fun lst : ((BIUList -> IUList) & (IUList -> UList) & (UList -> BIUList)) ->
                if lst is Nil
                then nil
                else snd lst

let regex_fail =

        fun lst : (BIUList -> IntList) ->
                if lst is Nil
                then nil
                else snd lst

let custom_sum =

        rec self x : (BIUList -> Int) ->
                if x is Nil
                then 0
                else if snd x is Nil
                then 0
                else
                        let b = fst x in
                        let tl = snd x in
                        let i = fst tl in
                        let tl = if snd tl is Nil then nil else snd (snd tl) in
                        if b is True
                        then plus i (self tl)
                        else self tl
