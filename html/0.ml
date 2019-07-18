(* Comparison with THF 10 *)

let incr = fun (Int -> Int) x -> magic

atom nil
type String = Nil | (Char, String)

let strlen = fun (Char -> Int) x -> magic

let is_int = fun (x : Any) ->
 if x is Int then true else false

let is_char = fun (x : Any) ->
 if x is Char then true else false

let not_ = fun (x : Any) ->
 if x is True then false else true

let or_ = fun (x : Any) ->
 if x is True then (fun (y : Any) -> true)
 else fun (y : Any) ->
      if y is True then true else false

let and_ = fun (x : Any) -> fun (y : Any) ->
  not_ (or_ (not_ x) (not_ y))

let add = fun (Int -> Int -> Int) x -> magic

let example14 =
  fun (input : Int|Char) -> fun (extra : Any) ->
  if and_ (is_int input) (is_int extra) is True
  then add (input) (extra)
  else
    if is_int extra is True
    then (debug "here" input)
    else 0
