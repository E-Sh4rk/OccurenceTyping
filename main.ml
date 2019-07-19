open Checker
open Main_common

let print_logs () =
  let treat (exprid, data)  =
    if data.visited = 0 && data.ignored > 0
    then Utils.warning data.position "Expression is unreachable!"
  in
  Seq.iter treat (all_logs ()) ;
  clear_logs ()

let print_ill_typed (pos, str) =
  Format.printf "Ill typed\n" ; Utils.error pos str

let _ =
    let fn = ref "test.ml" in
    if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;
    type_check_string (!fn) (print_string) print_logs print_ill_typed
