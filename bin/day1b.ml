(** [transform_digit cs] transforms the prefix of the char list [cs] into a
    digit and return a new char list. *)
let transform_digit list =
  match list with
  | [] -> []
  | _ :: [] -> list
  | _ :: _ :: [] -> list
  | 'o' :: 'n' :: 'e' :: xs -> '1' :: xs
  | 't' :: 'w' :: 'o' :: xs -> '2' :: xs
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: xs -> '3' :: xs
  | 'f' :: 'o' :: 'u' :: 'r' :: xs -> '4' :: xs
  | 'f' :: 'i' :: 'v' :: 'e' :: xs -> '5' :: xs
  | 's' :: 'i' :: 'x' :: xs -> '6' :: xs
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: xs -> '7' :: xs
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: xs -> '8' :: xs
  | 'n' :: 'i' :: 'n' :: 'e' :: xs -> '9' :: xs
  | _ -> list

(** [transform_digit_backward cs] transforms the prefix of the char list [cs]
    into a digit and return a new char list. But numbers are spelled backward.
    *)
let transform_digit_backward list =
  match list with
  | [] -> []
  | _ :: [] -> list
  | _ :: _ :: [] -> list
  | 'e' :: 'n' :: 'o' :: xs -> '1' :: xs
  | 'o' :: 'w' :: 't' :: xs -> '2' :: xs
  | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: xs -> '3' :: xs
  | 'r' :: 'u' :: 'o' :: 'f' :: xs -> '4' :: xs
  | 'e' :: 'v' :: 'i' :: 'f' :: xs -> '5' :: xs
  | 'x' :: 'i' :: 's' :: xs -> '6' :: xs
  | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: xs -> '7' :: xs
  | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: xs -> '8' :: xs
  | 'e' :: 'n' :: 'i' :: 'n' :: xs -> '9' :: xs
  | _ -> list

let is_digit c =
  match c with
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

(** [find_first cs] finds the first digit from a char list [cs]. *)
let rec find_first f list =
  match f list with
  | [] -> []
  | x :: xs -> if is_digit x then [x] else find_first f xs

let find_digits line =
  List.append (find_first transform_digit line) (find_first transform_digit_backward (List.rev line))

let parse_line line =
  Aoc.explode_string line
  |> find_digits
  |> Aoc.implode_string
  |> int_of_string

let main () =
  let%lwt score =
    Aoc.lwt_stdin_map parse_line
    |> Lwt_stream.fold (+)
    |> (|>) 0
  in
    Lwt_io.printf "%d\n" score

let () =
  Lwt_main.run @@ main ()
