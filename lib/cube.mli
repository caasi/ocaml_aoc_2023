(** The Cube module parses the cube game in day 2. *)

val eval : string -> (int * (int * int * int) list list) list
(** [eval s] parses string [s] into a list of game of cubes. *)
