
atom string
type Record = { ..}
type RecordWithField = { field=Int ..}

let add1 = fun (Int -> Int) x -> magic
let strlen = fun (String -> Int) x -> magic
let plus = fun (Int -> Int -> Int) x -> magic

let example1 = fun (x:Any) -> if x is Int then add1 x else 0

let example2 = fun (x:String|Int) -> if x is Int then add1 x else strlen x

let example3 = fun (x:Record) ->
	if x is RecordWithField then x.field else 0

let f = fun (x:Int|String) -> 0

let example4 = fun (x:Any) ->
	if x is Int then f x else if x is String then f x else 0

let example5_7 = fun (x:Any) -> (fun (y:Any) -> if x is Int
	then if y is String then plus x (strlen y) else 0
	else 0)

let example6 = fun (x:String|Int) -> (fun (y:Any) -> if x is Int
	then if y is String then plus x (strlen y) else strlen x
	else strlen x)

let strnum = fun (x:Any) ->
	if x is Int then true else
	if x is String then true else false

let example8 = fun (x:Any) -> if strnum x is True then f x else 0

let example10 = fun (x:(Any,Any)) -> if fst x is Int then add1 (fst x) else 7

let g = fun (x:(Int,Int)) -> true

let example11 = fun (x:(Any,Any)) -> if fst x is Int
	then if snd x is Int then g x else false
	else false

let carnum = fun (x:(Any,Any)) -> if fst x is Int then true else false

let example12 = fun (x:(Any,Any)) ->
	if carnum x is True then add1 (fst x) else 0
