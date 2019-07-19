open Checker
open Main_common
open Js_of_ocaml

let pr str =
  let output = Dom_html.getElementById "output" in
  output##.innerHTML :=
    Js.string ((Js.to_string output##.innerHTML) ^ str)

let prf fmt =
  Format.ksprintf pr fmt

let print_logs () =
  let treat (exprid, data)  =
    if data.visited = 0 && data.ignored > 0
    then
      prf "<span class='warning'>%s : %s</span>\n"
        (Position.string_of_pos data.position)
        "Expression is unreachable!"
  in
  Seq.iter treat (all_logs ()) ;
  clear_logs ()

let print_ill_typed (pos, str) =
  prf "<span class='error'>Ill-typed: %s : %s</span>\n"
  (Position.string_of_pos pos)
  str

let _ =
  let button = Dom_html.getElementById "checkButton" in
  Dom_html.addEventListener button Dom_html.Event.click
    (Dom_html.handler (fun _ ->
         match Dom_html.getElementById_coerce "code" Dom_html.CoerceTo.textarea with
         None -> Js._true
         | Some textArea ->  let txt = textArea##.value in
           type_check_string (Js.to_string txt) pr print_logs print_ill_typed;  Js._true)
    ) Js._false
