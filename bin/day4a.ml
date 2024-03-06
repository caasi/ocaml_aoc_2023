open Aoc

(* computations *)
let score (card : Card.t) =
  let s = Card.score card in
  if s = 0 then 0 else Int.shift_left 1 (s - 1)

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let game = Card.eval input in
  game |> List.map score |> List.fold_left ( + ) 0 |> Format.printf "%d\n" ;
  Format.print_flush () ;
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
