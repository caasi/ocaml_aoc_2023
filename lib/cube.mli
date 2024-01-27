(** The Cube module parses the cube game in day 2. *)

(** [eval s] parses string [s] into a list of game of cubes. *)
val eval : string -> (int * (int * int * int) list list) list

