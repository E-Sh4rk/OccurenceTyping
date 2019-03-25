
let dump_type t =
    Format.printf "%a\n" Cduce.dump t

let print_type t =
    Format.printf "%s\n" (Cduce.string_of_type t)
