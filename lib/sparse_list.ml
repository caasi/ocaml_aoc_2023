(** ['a indexed] is a value ['a] with an integer index. *)
type 'a indexed = int * 'a

type 'a t = 'a indexed list

(** [empty] is an empty sparse list. *)
let empty : 'a t = []

(** [update index f xs] updates the value at [index] of a sparse list [xs] to a
    update function [f]. *)
let rec update index f = function
  | [] ->
      [(index, f None)]
  | (i, y) :: tl when i = index ->
      (i, f (Some y)) :: tl
  | (i, y) :: tl when i > index ->
      (index, f None) :: (i, y) :: tl
  | hd :: tl ->
      hd :: update index f tl

(** [map_index f xs] maps a function [f] over a sparse list [xs] with an item
    and it's index. *)
let rec map_index f = function
  | [] ->
      []
  | (i, x) :: tl ->
      (i, f i x) :: map_index f tl

(** [fold_left f acc xs] folds a function [f] over a sparse list [xs] *)
let rec fold_left f acc = function
  | [] ->
      acc
  | (_, x) :: tl ->
      fold_left f (f acc x) tl

(** [get index xs] gets the value at [index] of a sparse list [xs]. *)
let rec get index = function
  | [] ->
      None
  | (i, _) :: _ when i > index ->
      None
  | (i, x) :: _ when i = index ->
      Some x
  | _ :: tl ->
      get index tl

let pp_print ?(pp_sep = Format.pp_print_cut) pp_v =
  Format.pp_print_list ~pp_sep (fun ppf (i, x) ->
      Format.fprintf ppf "(%i: %a)" i pp_v x )
