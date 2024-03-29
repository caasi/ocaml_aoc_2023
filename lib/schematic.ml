module Item2D = Item_2d

type item = Gap of int | Num of int * int | Sym

type row = item list

type t = row list

let to_item_2d_list schema =
  let rec aux_row acc y x = function
    | [] ->
        acc
    | Gap width :: rest ->
        aux_row acc y (x + width) rest
    | Num (value, width) :: rest ->
        aux_row (Item2D.Num (x, y, value, width) :: acc) y (x + width) rest
    | Sym :: rest ->
        aux_row (Item2D.Sym (x, y) :: acc) y (x + 1) rest
  in
  let rec aux acc y = function
    | [] ->
        acc
    | row :: rest ->
        aux (aux_row acc y 0 row) (y + 1) rest
  in
  aux [] 0 schema

(* parsers *)
open Angstrom
module P = Parser

let is_gap = function '.' -> true | _ -> false

let is_symbol c =
  (not (is_gap c)) && (not (P.is_digit c)) && c <> '\n' && c <> '\r'

let gap = lift (fun s -> Gap (String.length s)) (take_while1 is_gap)

let num =
  lift
    (fun s -> Num (int_of_string s, String.length s))
    (take_while1 P.is_digit)

let sym = lift (fun _ -> Sym) (skip is_symbol)

let row = many (gap <|> num <|> sym)

let schematic = sep_by end_of_line row

(* pretty printers *)
open Format

let pp_print_item fmt = function
  | Gap width ->
      fprintf fmt "Gap %d" width
  | Num (value, _) ->
      fprintf fmt "Num %d" value
  | Sym ->
      fprintf fmt "Sym"

let pp_print_row fmt row =
  let print_sep fmt () = pp_print_string fmt ", " ; pp_print_cut fmt () in
  pp_print_list ~pp_sep:print_sep pp_print_item fmt row

let pp_print_schematic fmt schema =
  pp_print_list ~pp_sep:pp_print_newline pp_print_row fmt schema

(* evaluator *)
let eval str =
  match parse_string ~consume:All schematic str with
  | Ok v ->
      v
  | Error msg ->
      failwith msg
