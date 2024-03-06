type t = int * int list * int list

let cid ((x, _, _) : t) = x

let score (card : t) =
  let _, winning_nums, own_nums = card in
  let init_nums = winning_nums |> List.map (fun n -> (n, ())) in
  let hash = Hashtbl.of_seq (List.to_seq init_nums) in
  let hit n = match Hashtbl.find_opt hash n with Some _ -> 1 | None -> 0 in
  List.fold_left (fun acc n -> acc + hit n) 0 own_nums

let is_digit = function '0' .. '9' -> true | _ -> false

(* parsers *)
open Angstrom

let spaces = skip_while (function ' ' -> true | _ -> false)

let num = take_while1 is_digit >>| int_of_string

let num_list = sep_by1 spaces num

let space = char ' '

let card_prefix = string "Card" *> spaces *> num <* string ":" <* spaces

let card =
  lift4
    (fun card xs _ ys -> (card, xs, ys))
    card_prefix num_list
    (spaces *> string "|" <* spaces)
    num_list

let game = sep_by1 end_of_line card

(* pretty printers *)
open Format

let list_sep fmt () = fprintf fmt ";@ "

let pp_print_winning_numbers fmt xs =
  fprintf fmt "Win: [%a]" (pp_print_list ~pp_sep:list_sep pp_print_int) xs

let pp_print_own_numbers fmt xs =
  fprintf fmt "Own: [%a]" (pp_print_list ~pp_sep:list_sep pp_print_int) xs

let pp_print_card fmt (card, xs, ys) =
  fprintf fmt "Card %d: %a, %a" card pp_print_winning_numbers xs
    pp_print_own_numbers ys

let pp_print_game fmt game =
  fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_newline pp_print_card) game

(* evaluator *)
let eval (str : string) =
  match parse_string ~consume:All game str with
  | Ok game ->
      game
  | Error msg ->
      failwith msg
