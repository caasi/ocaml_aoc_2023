(** The Aoc module provides helper functions and modules for daily problems. *)

(** The Cube module is for parsing cube games in day 2. *)
module Cube = Cube

module Schematic = Schematic
module Item2D = Item_2d
module SparseList = Sparse_list
module SparseMap = Sparse_map

(** [explode_string s] transforms string [s] into a char list. *)
let explode_string s = List.init (String.length s) (String.get s)

(** [implode_string cs] transforms a char list [cs] back to a string. *)
let implode_string l = String.of_seq (List.to_seq l)

(** [lwt_stdin_map f] reads lines from stdin and map each line with a function
    [f]. *)
let lwt_stdin_map f = Lwt_io.read_lines Lwt_io.stdin |> Lwt_stream.map f
