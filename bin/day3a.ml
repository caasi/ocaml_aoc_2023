open Aoc

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let schema = Schematic.eval input in
  Schematic.pp_print_schematic Format.std_formatter schema ;
  Format.print_flush () ;
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
