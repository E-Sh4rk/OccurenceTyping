
module CD = Cduce_types

(* Force linking with Cduce_core.Builtin for nicer pretty printing *)
let _Env = Cduce_core.Builtin.env

module LabelSet = CD.Ident.LabelSet
module LabelMap = CD.Ident.LabelMap

type typ = CD.Types.t
type node = CD.Types.Node.t

let simplify t = 
  (* Some arrow type seem to be printed really badly (with redudant arrows)
     when CDuce is used as a library. This is a workaround. *)
  let cap_arrow = function [] -> CD.Types.Function.any
    | (u, v) :: ll ->
      List.fold_left (fun acc (u, v) -> CD.Types.(cap acc (arrow (cons u) (cons v))))
    CD.Types.(arrow (cons u) (cons v)) ll     
  in
  let no_arrow = CD.Types.(diff t Function.any) in
  let arrow = CD.Types.(cap t Function.any) in
  let _, intf = CD.Types.Arrow.get arrow in 
  List.fold_left (fun acc line ->
      CD.Types.cup acc (cap_arrow line)
    ) no_arrow intf


let simplify = CD.Types.Cache.memo simplify

let pp fmt t = 
  CD.Types.Print.print fmt (simplify t)
let printf t = Format.printf "%a" CD.Types.Print.print t
let dump = CD.Types.dump
let string_of_type t = Format.asprintf "%a" CD.Types.Print.print t
let string_of_node t = Format.asprintf "%a" CD.Types.Print.print (CD.Types.descr t)
let descr = CD.Types.descr
let cons = CD.Types.cons


let any = CD.Types.any
let empty = CD.Types.empty
let any_node = cons any
let empty_node = cons empty


let cup = CD.Types.cup
let cap = CD.Types.cap
let diff = CD.Types.diff
let neg = CD.Types.neg

(* ----- *)

let to_label str = CD.Ident.Label.mk_ascii str
let from_label lbl = CD.Ident.Label.get_ascii lbl

(* ----- *)

(*
let mk_var internal name =
    let var = CD.Var.mk ~internal:internal name in
    CD.Types.var var
*)

let mk_atom ascii_name =
    ascii_name |> CD.AtomSet.V.mk_ascii |> CD.AtomSet.atom |> CD.Types.atom

(*
let mk_list alpha =
    let alpha_list = CD.Types.make () in

    let nil_atom = mk_atom "nil" in
    let cons = CD.Types.times alpha alpha_list in

    let descr = CD.Types.cup nil_atom cons in
    CD.Types.define alpha_list descr ;
    alpha_list
*)

let mk_new_typ = CD.Types.make

let define_typ = CD.Types.define

let normalize_typ = CD.Types.normalize


let mk_times = CD.Types.times

let pair_any = CD.Types.Times.any

let pi1 t =
  CD.Types.Product.pi1 (CD.Types.Product.get t)

let pi2 t =
  CD.Types.Product.pi2 (CD.Types.Product.get t)


let mk_record is_open fields =
  let fields = List.map (fun (str,node) -> (to_label str,node)) fields in
  let fields = LabelMap.from_list_disj fields in
  CD.Types.record_fields (is_open, fields)

let record_any = CD.Types.Rec.any

let absent = CD.Types.Absent.any

let any_or_absent = CD.Types.Record.any_or_absent

let absent_node = cons absent
let any_or_absent_node = cons any_or_absent
let or_absent = CD.Types.Record.or_absent

let empty_closed_record = CD.Types.empty_closed_record

let empty_open_record = CD.Types.empty_open_record

let get_field record field =
  CD.Types.Record.project record (to_label field)

let all_labels r =
  let accu = ref CD.Ident.LabelSet.empty in
  List.iter (fun (fields, _, _) ->
    CD.Ident.LabelMap.iteri (fun i _ -> 
      accu := CD.Ident.LabelSet.add i !accu) fields    
    ) (CD.Types.Record.get r);
    !accu


let all_fields record =
  let lbls = all_labels record in
  List.map from_label (LabelSet.get lbls)

let merge_records = CD.Types.Record.merge

let remove_field record field =
  CD.Types.Record.remove_field record (to_label field)


let is_empty = CD.Types.is_empty
let non_empty = CD.Types.non_empty
let subtype = CD.Types.subtype
let disjoint = CD.Types.disjoint
let equiv = CD.Types.equiv


(* Maybe not optimised (if no memoisation for Arrow.get). We'll see that later. *)
let mk_arrow = CD.Types.arrow

let arrow_any = CD.Types.Function.any

let domain t =
    if subtype t arrow_any then
      let t = CD.Types.Arrow.get t in
      CD.Types.Arrow.domain t
    else empty

let apply t args =
    let t = CD.Types.Arrow.get t in
    CD.Types.Arrow.apply t args

let dnf t =
    snd (CD.Types.Arrow.get t)


let true_typ = mk_atom "true"
let false_typ = mk_atom "false"
let bool_typ = CD.Builtin_defs.bool
let int_typ = CD.Builtin_defs.int
let char_typ = CD.Builtin_defs.char
let unit_typ = mk_atom "unit"

let interval i1 i2 =
  match i1, i2 with
  | Some i1, Some i2 -> 
    let i1 = CD.Intervals.V.from_int i1 in
    let i2 = CD.Intervals.V.from_int i2 in
    let i = CD.Intervals.bounded i1 i2 in
    CD.Types.interval i
  | Some i1, None ->
    let i1 = CD.Intervals.V.from_int i1 in
    let i = CD.Intervals.right i1 in
    CD.Types.interval i
  | None, Some i2 ->
    let i2 = CD.Intervals.V.from_int i2 in
    let i = CD.Intervals.left i2 in
    CD.Types.interval i
  | None, None ->
    CD.Types.Int.any
    
let single_char c =
  let c = CD.CharSet.V.mk_char c in
  let c = CD.CharSet.atom c in
  CD.Types.char c
