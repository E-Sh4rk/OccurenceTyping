(* Prelude *)
atom string
atom no

let and_ = fun (x : Any) ->
           fun (y : Any) -> if x is True then if y is True then y else false else false

let and2_ = fun (x : (Any,Any)) -> if fst x is True then if snd x is True then fst x else false else false

let or_ = fun (x : Any) ->
fun (y:Any) -> if x is True then true else if y is True then true else false

let not_ = fun (x : Any) -> if x is True then true else false
let add1 = fun (Int -> Int) x -> magic

let is_string = fun (x : Any) ->
  if x is String then true else false

let is_int = fun (x : Any) ->
  if x is Int then true else false

let strlen = fun ((String) -> Int) x -> magic

let add = fun (Int -> Int -> Int ) x -> magic

let f = fun ((Int | String) -> Int) x -> magic

let g = fun ((Int, Int) -> Int) x -> magic

(* examples *)

let example1 = fun (x:Any) ->
     if x is Int then add1 x else 0


let example2 = fun (x:String|Int) ->
  if x is Int then add1 x else strlen x


let example3 = fun (x: Any) ->
    if x is (Any \ False) then (x,x) else false


let example4 = fun (x : Any) ->
   if or_ (is_int x) (is_string x) is True then x else 'A'


let example5 = fun (x : Any) -> fun (y : Any) ->
   if and_ (is_int x) (is_string y) is True then
    add x (strlen y) else 0


let example6 = fun (x : Int|String) -> fun (y : Any) ->
if and_ (is_int x) (is_string y) is True then
  add  x (strlen y) else strlen x


let example7 = fun (x : Any) -> fun (y : Any) ->
if  (if (is_int x) is True then (is_string y) else false) is True then
  add x (strlen y) else 0


let example8 = fun (x : Any) -> if or_ (is_int x) (is_string x) is True then true else false


let example9 = fun (x : Any) ->
if
  (if is_int x is True then is_int x else is_string x)
  is True then  f x else 0


let example10 = fun (p : (Any,Any)) ->
   if is_int (fst p) is True then add1 (fst p) else 7


let example11 = fun (p : (Any, Any)) ->
   if and_ (is_int (fst p)) (is_int (snd p)) is True then g p else no


let example12 = fun (p : (Any, Any)) -> if is_int (fst p) is True then true else false


let example13 =
    fun (x : Any) ->
      fun (y : Any) ->
       if and_ (is_int x) (is_string y) is True then 1
       else if is_int x is True then 2
       else 3

let example14_alt = fun (input : Int | String) -> fun (extra : (Any, Any)) ->
    if and2_ ((is_int input),(is_int (fst extra)))  is True then
       add input (fst extra)
    else if (is_int input,is_int (fst extra)) is (Any,True) then add (strlen input) (fst extra)
    else 0
