(** ['a indexed] is a value ['a] with an integer index. *)
type 'a indexed = int * 'a

type 'a t = 'a indexed list

(** [empty] is an empty sparse list. *)
let empty : 'a t = []

(** [update index x xs] updates the value at [index] of a sparse list [xs] to a
    new value [x]. *)
let rec update index x = function
  | [] ->
      [(index, x)]
  | (i, _) :: tl when i = index ->
      (i, x) :: tl
  | (i, y) :: tl when i < index ->
      (i, y) :: (index, x) :: tl
  | hd :: tl ->
      hd :: update index x tl

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
