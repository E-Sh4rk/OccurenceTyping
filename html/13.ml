atom null
type Object = Null | { prototype = Object ..}
type ObjectWithPropertyL = { l = Any ..}
  | { prototype = ObjectWithPropertyL ..}

let has_property_l = fun (o:Object) ->
    if o is ObjectWithPropertyL then true else false

let has_own_property_l = fun (o:Object) ->
    if o is { l=Any ..} then true else false


type X = X -> Object -> Any
let z = fun (((Object -> Any) -> Object -> Any ) -> (Object -> Any)) f ->
      let delta = fun (X -> (Object -> Any) ) x ->
         f ( fun (Object -> Any) v -> ( x x v ))
       in delta delta  

let get_property_l =
    let aux = fun (self:Object->Any) -> fun (o:Object)->
        if has_own_property_l o is True then o.l
        else if o is Null then null
        else self (o.prototype)
    in z aux