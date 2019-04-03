
let dump_type t =
    Format.printf "%a\n" Cduce.dump t

let print_type t =
    Format.printf "%s\n" (Cduce.string_of_type t)

let memoize f input_transform ht =
  let rec aux input =
    let htbl_key = input_transform input in
    if Hashtbl.mem ht htbl_key then Hashtbl.find ht htbl_key
    else
    (
      let res = f aux input in
      Hashtbl.replace ht htbl_key res ;
      res
    )
  in aux

let no_memoize f =
  let rec aux input =
    f aux input
  in aux