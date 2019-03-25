
open Cduce

let alpha = mk_var false "a"
let beta  = mk_var false "b"

let _ =
    let alpha_list = mk_list (cons alpha) in
    let beta_list = mk_list (cons beta) in
    let beta_list_list = mk_list beta_list in
    let union = cup (descr alpha_list) (descr beta_list_list) in
    printf union
