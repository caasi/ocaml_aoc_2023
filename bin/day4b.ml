open Aoc

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let game = Card.eval input in
  let length = List.length game in
  let count_table = Hashtbl.create length in
  let rec aux card =
    let cid = Card.cid card in
    match Hashtbl.find_opt count_table cid with
    | Some count ->
        count
    | None ->
        let s = Card.score card in
        let ranges = range (cid + 1) (cid + 1 + s) in
        let count =
          List.fold_left
            (fun acc n ->
              acc + if n >= length then 0 else aux (List.nth game (n - 1)) )
            s ranges
        in
        Hashtbl.add count_table cid count ;
        count
  in
  let total = length + (game |> List.fold_left (fun acc c -> acc + aux c) 0) in
  Format.printf "%d\n" total ; Format.print_flush () ; Lwt.return_unit

let () = Lwt_main.run @@ main ()
