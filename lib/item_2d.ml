type t = Num of int * int * int * int | Sym of int * int

(* getters *)
let value = function Num (_, _, value, _) -> value | Sym (_, _) -> 1

(* pretty printers *)
open Format

let pp_print_item_coord fmt = function
  | Num (x, y, value, width) ->
      fprintf fmt "NumCoord (%d, %d, %d, %d)" x y value width
  | Sym (x, y) ->
      fprintf fmt "SymCoord (%d, %d)" x y

let pp_print_item_coords fmt coords =
  pp_print_list pp_print_item_coord fmt coords

(* features *)
let surrounded_points = function
  | Num (x, y, _, width) ->
      List.concat
        [ List.init (width + 2) (fun i -> (x + i - 1, y - 1))
        ; [(x - 1, y); (x + width, y)]
        ; List.init (width + 2) (fun i -> (x + i - 1, y + 1)) ]
  | Sym (_, _) ->
      []
