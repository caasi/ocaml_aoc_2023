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

let pp_print ?(pp_sep = Format.pp_print_space) pp_v =
  Format.pp_print_list ~pp_sep (fun ppf (i, x) ->
      Format.fprintf ppf "(%i: %a)" i pp_v x )
