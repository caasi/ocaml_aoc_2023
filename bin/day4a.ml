open Aoc

(* computations *)
let score (card : Card.t) =
  let _, winning_nums, own_nums = card in
  let init_nums = winning_nums |> List.map (fun n -> (n, true)) in
  let hash = Hashtbl.of_seq (List.to_seq init_nums) in
  let rec aux nums =
    match nums with
    | [] ->
        0
    | n :: ns -> (
      match Hashtbl.find_opt hash n with Some _ -> 1 + aux ns | None -> aux ns )
  in
  let match_count = aux own_nums in
  if match_count = 0 then 0 else Int.shift_left 1 (match_count - 1)

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let game = Card.eval input in
  game |> List.map score |> List.fold_left ( + ) 0 |> Format.printf "%d\n" ;
  Format.print_flush () ;
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
