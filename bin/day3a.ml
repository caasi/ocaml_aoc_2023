open Aoc

(* calculations *)
let sparse_map_of_item_2d_list list =
  let open Schematic in
  let rec aux acc = function
    | [] ->
        acc
    | Item2D.Sym (x, y) :: tl ->
        aux (SparseMap.update x y (fun _ -> true) acc) tl
    | _ :: tl ->
        aux acc tl
  in
  aux SparseMap.empty list

let hit_test sparse_map item =
  let hit (x, y) =
    match SparseMap.get x y sparse_map with None -> false | Some _ -> true
  in
  let coords = Item2D.surrounded_points item in
  List.map hit coords |> List.exists (fun x -> x)

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let schema = Schematic.eval input in
  let item_list = Schematic.to_item_2d_list schema in
  let sparse_map = sparse_map_of_item_2d_list item_list in
  let sum =
    item_list
    |> List.fold_left
         (fun acc item ->
           if hit_test sparse_map item then acc + Item2D.value item else acc )
         0
  in
  Lwt_io.printf "%d\n" sum

let () = Lwt_main.run @@ main ()
