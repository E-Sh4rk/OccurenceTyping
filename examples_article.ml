let incr = fun (Int->Int) x -> magic
let lnot = fun (Bool->Bool) x -> magic
atom nil

let basic_inf = fun (y : Int | Bool ) ->
    if y is Int then incr y else lnot y

let any_inf = fun (x : Any) ->
    if x is Int then incr x else
    if x is Bool then lnot x else x

let is_int = fun (x : Any) ->
    if x is Int then true else false

let is_bool = fun (x : Any) ->
    if x is Bool then true else false

let is_char = fun (x : Any) ->
    if x is Char then true else false

let not_ = fun (x : Any) ->
    if x is True then false else true

let or_ = fun (x : Any) -> fun (y: Any) ->
    if x is True then true
    else if y is True then true else false

let and_ = fun (x : Any) -> fun (y : Any) ->
    if not_ (or_ ( not_ x) ( not_ y)) is True
    then true else false

let f = fun (x : Any) -> fun (y : Any) ->
    if and_ ( is_int x) ( is_bool y) is True
    then 1 else
    if or_ ( is_char x) ( is_int y) is True
    then 2 else 3

let test_1 = f 3 true
let test_2 = f (42 ,42) 42
let test_3 = f nil nil

type Document = { nodeType =9 ..}
and Element = { nodeType =1, childNodes = NodeList ..}
and Text = { nodeType =3, isElementContentWhiteSpace = Bool ..}
and Node = Document | Element | Text
and NodeList = Nil | (Node , NodeList )

let is_empty_node = fun (x : Node ) ->
    if x. nodeType is 9 then false
    else if x. nodeType is 3 then
        x. isElementContentWhiteSpace
    else
        if x. childNodes is Nil then true else false

let xor_ = fun (x : Any) -> fun (y : Any) ->
    if and_ (or_ x y) ( not_ ( and_ x y)) is True
    then true else false

(* f, g have type : (Int ->Int) & (Any -> Bool ) *)
let f = fun ((Int ->Int) & (Any -> Bool )) x -> magic
let g = fun ((Int ->Int) & (Any -> Bool )) x -> magic

let example10 = fun (x : Any) ->
    if (f x, g x) is (Int , Bool ) then 1 else 2
