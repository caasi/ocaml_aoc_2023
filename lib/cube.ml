open Angstrom

let is_digit =
  function '0' .. '9' -> true | _ -> false

(* parsers *)
let digits = take_while1 is_digit
let space = char ' '
let sep_round = string ", "
let sep_game = string "; "
let prefix_game = string "Game " *> digits <* string ": "

let red =
  lift (fun x -> (int_of_string x, 0, 0))
    digits <* space <* string "red"

let green =
  lift (fun x -> (0, int_of_string x, 0))
    digits <* space <* string "green"

let blue =
  lift (fun x -> (0, 0, int_of_string x))
    digits <* space <* string "blue"

let round = sep_by1 sep_round (red <|> green <|> blue)

let game =
  lift2 (fun id rounds -> (int_of_string id, rounds))
    prefix_game
    (sep_by1 sep_game round)

let games = sep_by1 end_of_line game

let eval (str:string) =
  match parse_string ~consume:All games str with
  | Ok v      -> v
  | Error msg -> failwith msg

