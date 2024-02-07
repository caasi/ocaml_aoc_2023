open Aoc

(* computations *)

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let game = Card.eval input in
  Card.pp_print_game Format.std_formatter game ;
  Format.print_flush () ;
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
