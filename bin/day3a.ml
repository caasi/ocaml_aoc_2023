open Aoc

(* calculations *)
let sparse_map_of_item_coord_list list =
  let open Schematic in
  let rec aux acc = function
    | [] ->
        acc
    | SymCoord (x, y) :: tl ->
        aux (SparseMap.update x y true acc) tl
    | _ :: tl ->
        aux acc tl
  in
  aux SparseMap.empty list

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let schema = Schematic.eval input in
  let item_coord_list = Schematic.item_coord_of_schematic schema in
  let sparse_map = sparse_map_of_item_coord_list item_coord_list in
  ( match SparseMap.get 133 138 sparse_map with
  | None ->
      ()
  | Some _ ->
      print_endline "found a symbol at (133, 138)" ) ;
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
