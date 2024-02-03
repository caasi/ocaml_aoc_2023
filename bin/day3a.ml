open Aoc

(* calculations *)
let sparse_map_of_item_coord_list list =
  let open Schematic in
  let rec aux acc = function
    | [] ->
        acc
    | SymCoord (x, y) :: tl ->
        aux (SparseMap.update x y (fun _ -> true) acc) tl
    | _ :: tl ->
        aux acc tl
  in
  aux SparseMap.empty list

let hit_test sparse_map item =
  let hit (x, y) =
    match SparseMap.get x y sparse_map with None -> false | Some _ -> true
  in
  let coords = Schematic.range_of_item_coord item in
  List.map hit coords |> List.exists (fun x -> x)

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let schema = Schematic.eval input in
  let item_coord_list = Schematic.item_coord_of_schematic schema in
  let sparse_map = sparse_map_of_item_coord_list item_coord_list in
  let sum =
    item_coord_list
    |> List.fold_left
         (fun acc item ->
           if hit_test sparse_map item then acc + Schematic.get_value item
           else acc )
         0
  in
  Lwt_io.printf "%d\n" sum

let () = Lwt_main.run @@ main ()
