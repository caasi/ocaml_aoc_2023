(** The Cube module parses the cube game in day 2. *)

type color = int * int * int

type round = color list

type game = int * round list

type games = game list

val pp_print_games : Format.formatter -> games -> unit

val eval : string -> games
(** [eval s] parses string [s] into a list of game of cubes. *)
