let is_digit c =
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

(** [find_last cs default] finds the last digit from a list of char [cs] or
    fallback to a default list of only 1 char [default]. *)
let rec find_last list result =
  match list with
  | [] -> result
  | x :: xs -> find_last xs (if is_digit x then [x] else result)

(** [find_digits cs] finds the first digit and the last digit from a list of
    char [cs] and return a list of only 2 digits. *)
let rec find_digits list =
  match list with
  | [] -> []
  | x :: xs -> if is_digit x then x :: find_last xs [x] else find_digits xs

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
