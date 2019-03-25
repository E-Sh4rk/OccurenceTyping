
module CD = Cduce_lib

type typ = CD.Types.t
type node = CD.Types.Node.t

let printf = CD.Types.Print.printf
let descr = CD.Types.descr
let cons = CD.Types.cons

let cup = CD.Types.cup
let cap = CD.Types.cap

let mk_var internal name =
    let var = CD.Var.mk ~internal:internal name in
    CD.Types.var var

let mk_atom ascii_name =
    ascii_name |> CD.Atoms.V.mk_ascii |> CD.Atoms.atom |> CD.Types.atom

let mk_list alpha =
    let alpha_list = CD.Types.make () in

    let nil_atom = mk_atom "nil" in
    let cons = CD.Types.times alpha alpha_list in

    let descr = CD.Types.cup nil_atom cons in
    CD.Types.define alpha_list descr ;
    alpha_list
