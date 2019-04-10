(* Basic functions *)

let incr =
    fun (Int -> Int) i -> magic

let plus =
    fun (Int -> Int -> Int) i -> magic

let lnot =
    fun ((True -> False) & (False -> True)) x -> magic

let land =
    fun (Bool -> Bool -> Bool) x -> magic

(* Basic example (first example of the paper) *)

let basic_ok =

    let bti =
        fun (Bool -> Int) b -> magic
    in

    fun ((Int | Bool) -> Int) x ->
        if x is Int then incr x else bti x

let basic_fail =

    let bti =
        fun (Bool -> Int) b -> magic
    in

    fun ((Int | Bool) -> Int) x ->
        if x is Any then incr x else bti x

(* Example with intervals *)

let interval =

        fun (-5--5 -> -1 | 1) x ->
                if x is 0--5 then 1
                else if x is -5---1 then -1
                else false

(* Example with Let and Pairs *)

let let_pairs_ok =

    fun (((Int, Int) -> Int) & ((Bool, Bool) -> Bool)) x ->
        let y = fst x in let z = snd x in
        if x is (Int, Any) then plus y z else land (lnot y) z

(* Example that shows that backtyping is stronger if we intersect with the environment as soon as possible *)
(* Also need the improved version of the square operator *)

let two_steps =

        let f = fun (( Any\Int -> (Any, Any)\(Int,Int) ) & ( Int -> (Int,Int) )) x -> magic
        in
        fun (Any -> Int) x ->
                if snd (f x) is Int
                then
                        if fst (f x) is Int then x
                        else 0
                else 0

(* Example with recursive types and lists *)

atom nil
type List = Nil | (Any, List)
type IntList = Nil | (Int, IntList)
type BoolList = Nil | (Bool, BoolList)

let lists =

        fun (BoolList|IntList -> IntList) lst ->
                if lst is Nil
                then nil
                else if fst lst is Int
                then lst
                else nil

type BIUList = Nil | (Bool, IUList)
and  IUList  = Nil | (Int, UList)
and  UList   = Nil | (Unit, BIUList)

let regex_next =

        fun ((BIUList -> IUList) & (IUList -> UList) & (UList -> BIUList)) lst ->
                if lst is Nil
                then nil
                else snd lst

let regex_fail =

        fun (BIUList -> IntList) lst ->
                if lst is Nil
                then nil
                else snd lst

let custom_sum =

        fun (self:BIUList -> Int) x ->
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


(* Some tests of the paper *)

let appl1_fail =

   fun ( ((Int -> Int) | (Bool -> Bool)) -> (Int | Bool) -> (Int | Bool)) x1 ->
     fun ( (Int | Bool) -> (Int | Bool) ) x2 ->
         if (x1 x2) is Int then plus x2 (x1 x2) else land x2 (x1 x2)

let appl1_ok =

   fun ( ((Int -> Int) & (Bool -> Bool)) -> (Int | Bool) -> (Int | Bool)) x1 ->
     fun ( (Int | Bool) -> (Int | Bool) ) x2 ->
         if (x1 x2) is Int then plus x2 (x1 x2) else land x2 (x1 x2)

let appl2 =

   let bti =
         fun (Bool -> Int) b -> magic
   in
   fun ( ( (Int|Char -> Int) | (Bool|Char -> Bool) ) -> Char -> Int) x1 ->
        fun (Char -> Int) x2 ->
                if (x1 x2) is Int then incr (x1(x1 x2)) else bti (x1(x1 x2))
