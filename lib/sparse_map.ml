module SL = Sparse_list

let empty = SL.empty

let update x y value map =
  let new_row = match SL.get y map with Some row -> row | None -> SL.empty in
  SL.insert_or_replace y (SL.insert_or_replace x value new_row) map

let get x y map =
  match SL.get y map with Some row -> SL.get x row | None -> None
