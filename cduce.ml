
module CD = Cduce_lib
module LabelSet = CD.Ident.LabelSet
module LabelMap = CD.Ident.LabelMap

type typ = CD.Types.t
type node = CD.Types.Node.t


let printf = CD.Types.Print.printf
let dump = CD.Types.Print.dump
let string_of_type = CD.Types.Print.string_of_type
let string_of_node = CD.Types.Print.string_of_node
let descr = CD.Types.descr
let cons = CD.Types.cons


let cup = CD.Types.cup
let cap = CD.Types.cap

(* ----- *)

let to_label str = CD.Ident.Label.mk_ascii str
let from_label lbl = CD.Ident.Label.get_ascii lbl

(* ----- *)

let mk_var internal name =
    let var = CD.Var.mk ~internal:internal name in
    CD.Types.var var

let mk_atom ascii_name =
    ascii_name |> CD.Atoms.V.mk_ascii |> CD.Atoms.atom |> CD.Types.atom

let mk_record opened fields =
    let fields = List.map (fun (str,node) -> (to_label str,node)) fields in
    let fields = LabelMap.from_list_disj fields in
    CD.Types.record_fields (opened, fields)

let mk_list alpha =
    let alpha_list = CD.Types.make () in

    let nil_atom = mk_atom "nil" in
    let cons = CD.Types.times alpha alpha_list in

    let descr = CD.Types.cup nil_atom cons in
    CD.Types.define alpha_list descr ;
    alpha_list


let get_field record field =
    CD.Types.Record.project record (to_label field)

let all_fields record =
    let lbls = CD.Types.Record.all_labels record in
    List.map from_label (LabelSet.get lbls)

    
let is_empty = CD.Types.is_empty
let non_empty = CD.Types.non_empty
let subtype = CD.Types.subtype
let disjoint = CD.Types.disjoint
let equiv = CD.Types.equiv
