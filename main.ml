
module CD = Cduce_lib

let alpha =
    "a" |> CD.Var.mk |> CD.Types.var |> CD.Types.cons

let alpha_list =
    let alpha_list = CD.Types.make () in

    let nil_atom = "nil" |> CD.Atoms.V.mk_ascii |> CD.Atoms.atom |> CD.Types.atom in
    let cons = CD.Types.times alpha alpha_list in

    let descr = CD.Types.cup nil_atom cons in
    CD.Types.define alpha_list descr ;
    alpha_list

let _ = CD.Types.Print.printf (CD.Types.descr alpha_list)
