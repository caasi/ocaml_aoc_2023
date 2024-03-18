open Angstrom

let is_digit = function '0' .. '9' -> true | _ -> false

let space = char ' '

let spaces = skip_while (function ' ' -> true | _ -> false)

let digits = take_while1 is_digit

let num = digits >>| int_of_string

let digits_list = sep_by1 spaces digits

let num_list = sep_by1 spaces num
