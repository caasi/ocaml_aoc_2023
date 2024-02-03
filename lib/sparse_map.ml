module SL = Sparse_list

type 'a t = 'a SL.t SL.t

(** [empty] is an empty sparse map. *)
let empty : 'a t = SL.empty

(** [update x y f map] updates the value at ([x], [y]) of a 2D sparse map
    [map] with a update function [f]. *)
let update x y f map =
  let update_with f = function None -> f SL.empty | Some row -> f row in
  SL.update y (update_with @@ SL.update x f) map

(** [get x y map] gets the value at ([x], [y]) of a 2D sparse map [map]. *)
let get x y map =
  match SL.get y map with Some row -> SL.get x row | None -> None

let pp_print ?(pp_sep = Format.pp_print_cut) pp_v =
  SL.pp_print ~pp_sep (SL.pp_print ~pp_sep pp_v)
