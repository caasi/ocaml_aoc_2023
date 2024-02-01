open Aoc

let sparse_map_of_item_coord_list list =
  let open Schematic in
  let rec aux acc = function
    | [] ->
        acc
    | SymCoord (x, y) :: tl ->
        let new_map =
          let new_row =
            match SparseList.get y acc with Some row -> row | None -> []
          in
          SparseList.insert_or_replace y
            (SparseList.insert_or_replace x true new_row)
            acc
        in
        aux new_map tl
    | _ :: tl ->
        aux acc tl
  in
  aux [] list

let sparse_map_get x y map =
  match SparseList.get y map with
  | Some row ->
      SparseList.get x row
  | None ->
      None

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let schema = Schematic.eval input in
  let item_coord_list = Schematic.item_coord_of_schematic schema in
  let sparse_map = sparse_map_of_item_coord_list item_coord_list in
  ( match sparse_map_get 133 138 sparse_map with
  | None ->
      ()
  | Some _ ->
      print_endline "found a symbol at (133, 138)" ) ;
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
