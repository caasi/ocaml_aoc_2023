type 'a indexed = int * 'a

type 'a t = 'a indexed list

let rec insert_or_replace index x = function
  | [] ->
      [(index, x)]
  | (i, _) :: tl when i = index ->
      (i, x) :: tl
  | (i, y) :: tl when i < index ->
      (i, y) :: (index, x) :: tl
  | hd :: tl ->
      hd :: insert_or_replace index x tl

let rec get index = function
  | [] ->
      None
  | (i, _) :: _ when i > index ->
      None
  | (i, x) :: _ when i = index ->
      Some x
  | _ :: tl ->
      get index tl