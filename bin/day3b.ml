open Aoc

(* calculations *)
let sparse_map_of_item_2d_list list =
  let open Schematic in
  let rec aux acc = function
    | [] ->
        acc
    | Item2D.Sym (x, y) :: tl ->
        aux (SparseMap.update x y (fun _ -> []) acc) tl
    | _ :: tl ->
        aux acc tl
  in
  aux SparseMap.empty list

let hit sparse_map item =
  let points = Item2D.surrounded_points item in
  let test_points x y =
    points |> List.map (fun (x1, y1) -> x = x1 && y = y1) |> List.exists Fun.id
  in
  SparseMap.map_index
    (fun x y hits -> if test_points x y then Item2D.value item :: hits else hits)
    sparse_map

(* main *)
let main () =
  let%lwt input = Lwt_io.read Lwt_io.stdin in
  let schema = Schematic.eval input in
  let item_list = Schematic.to_item_2d_list schema in
  let sparse_map =
    let init_map = sparse_map_of_item_2d_list item_list in
    List.fold_left hit init_map item_list
  in
  let add_hits acc hits =
    if List.length hits = 2 then hits |> List.fold_left ( * ) 1 |> ( + ) acc
    else acc
  in
  let sum = SparseMap.fold_left add_hits 0 sparse_map in
  Lwt_io.printf "%d\n" sum

let () = Lwt_main.run @@ main ()
