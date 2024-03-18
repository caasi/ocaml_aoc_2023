open Angstrom

type color = int * int * int

type round = color list

type game = int * round list

type games = game list

(* parsers *)
module P = Parser

let sep_round = string ", "

let sep_game = string "; "

let prefix_game = string "Game " *> P.digits <* string ": "

let red =
  lift (fun x -> (int_of_string x, 0, 0)) P.digits <* P.space <* string "red"

let green =
  lift (fun x -> (0, int_of_string x, 0)) P.digits <* P.space <* string "green"

let blue =
  lift (fun x -> (0, 0, int_of_string x)) P.digits <* P.space <* string "blue"

let round = sep_by1 sep_round (red <|> green <|> blue)

let game =
  lift2
    (fun id rounds -> (int_of_string id, rounds))
    prefix_game (sep_by1 sep_game round)

let games = sep_by1 end_of_line game

(* pretty printers *)
open Format

let pp_print_color fmt (r, g, b) = fprintf fmt "(R: %d, G: %d, B: %d)" r g b

let pp_print_round fmt round =
  fprintf fmt "[" ;
  pp_open_box fmt 1 ;
  let print_sep fmt () = pp_print_string fmt ", " ; pp_print_cut fmt () in
  pp_print_list ~pp_sep:print_sep pp_print_color fmt round ;
  pp_close_box fmt () ;
  fprintf fmt "]"

let pp_print_game fmt (id, rounds) =
  fprintf fmt "Game %d: " id ;
  pp_open_box fmt 1 ;
  let print_sep fmt () = pp_print_string fmt "; " ; pp_print_cut fmt () in
  pp_print_list ~pp_sep:print_sep pp_print_round fmt rounds ;
  pp_close_box fmt ()

let pp_print_games fmt games =
  pp_print_list ~pp_sep:pp_print_newline pp_print_game fmt games ;
  pp_print_newline fmt ()

(* evaluator *)
let eval (str : string) =
  match parse_string ~consume:All games str with
  | Ok v ->
      v
  | Error msg ->
      failwith msg
