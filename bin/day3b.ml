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

open Lens

(** [list_apply ls] creates a lens from lenses [ls]. Each lens of [ls] is
    focused on the same input. So you can get multiple values as a list from it
    or update it with a value list. All new values will be updated to the same
    input. *)
let list_apply ls =
  { get= (fun a -> List.map (fun l -> l.get a) ls)
  ; set= (fun bs a -> List.fold_left2 (fun acc l b -> l.set b acc) a ls bs) }

let hit sparse_map item =
  let points = Item2D.surrounded_points item in
  (* a lens list to access different [points] of the same map *)
  let lens_list = points |> List.map (fun (x, y) -> SparseMap.at x y) in
  (* a function to update just one hits inside a sparse map *)
  let update_hits opt =
    match opt with Some hits -> Some (Item2D.value item :: hits) | None -> opt
  in
  let open Lens in
  let open Infix in
  sparse_map |> list_apply lens_list ^%= List.map update_hits

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
