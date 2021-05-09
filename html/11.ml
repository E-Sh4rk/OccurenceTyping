atom null
type Object = Null | { prototype = Object ..}
type ObjectWithPropertyL = { l = Any ..}
  | { prototype = ObjectWithPropertyL ..}

let has_property_l = fun (o:Object) ->
    if o is ObjectWithPropertyL then true else false

let has_own_property_l = fun (o:Object) ->
    if o is { l=Any ..} then true else false

let get_property_l = fun (self:Object->Any) o ->
    if has_own_property_l o is True then o.l
    else if o is Null then null
    else self (o.prototype)