module SL = Sparse_list

type 'a t = 'a SL.t SL.t

(** [empty] is an empty sparse map. *)
let empty : 'a t = SL.empty

(** [update x y f map] updates the value at ([x], [y]) of a 2D sparse map
    [map] with a update function [f]. *)
let update x y f map =
  let update_with f = function None -> f SL.empty | Some row -> f row in
  SL.update y (update_with @@ SL.update x f) map

(** [map f map] maps a function [f] over a 2D sparse map [map]. *)
let map_index f =
  let go g y = g (fun x -> f x y) in
  SL.map_index (SL.map_index |> go)

(** [fold_left f acc map] folds a function [f] over a 2D sparse map [map]. *)
let fold_left f = SL.fold_left (SL.fold_left f)

(** [get x y map] gets the value at ([x], [y]) of a 2D sparse map [map]. *)
let get x y map =
  match SL.get y map with Some row -> SL.get x row | None -> None

(* pretty printers *)
let pp_print ?(pp_sep = Format.pp_print_cut) pp_v =
  SL.pp_print ~pp_sep (SL.pp_print ~pp_sep pp_v)

(* lenses *)
open Lens

(** [at x y map] creates a lens to focus on a value at (x, y) of a [map]. *)
let at x y =
  { get=
      (fun map ->
        match SL.get y map with Some row -> SL.get x row | None -> None )
  ; set=
      (fun opt map ->
        match opt with None -> map | Some v -> update x y (fun _ -> v) map ) }
