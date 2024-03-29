open Aoc

(* computations *)
let compare_colors (r1, g1, b1) (r2, g2, b2) =
  ( (if r1 > r2 then r1 else r2)
  , (if g1 > g2 then g1 else g2)
  , if b1 > b2 then b1 else b2 )

let merge_colors = List.fold_left compare_colors (0, 0, 0)

let merge_rounds = merge_colors

let merge_game (id, rounds) = (id, merge_rounds (List.map merge_colors rounds))

let get_power (r, g, b) = r * g * b

let sum_games = List.fold_left (fun acc (_, c) -> acc + get_power c) 0

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let games = List.map merge_game (Cube.eval input) in
  let sum = sum_games games in
  Lwt_io.printf "%d" sum

let () = Lwt_main.run @@ main ()
