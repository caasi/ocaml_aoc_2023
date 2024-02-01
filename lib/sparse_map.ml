module SL = Sparse_list

type 'a t = 'a SL.t SL.t

(** [empty] is an empty sparse map. *)
let empty : 'a t = SL.empty

(** [update x y value map] updates the value at ([x], [y]) of a 2D sparse map
    [map] with a new value [value]. *)
let update x y value map =
  let new_row = match SL.get y map with Some row -> row | None -> SL.empty in
  SL.update y (SL.update x value new_row) map

(** [get x y map] gets the value at ([x], [y]) of a 2D sparse map [map]. *)
let get x y map =
  match SL.get y map with Some row -> SL.get x row | None -> None
