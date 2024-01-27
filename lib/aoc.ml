module Cube = Cube

let explode_string s = List.init (String.length s) (String.get s)

let implode_string l = String.of_seq (List.to_seq l)

let lwt_stdin_map f = Lwt_io.read_lines Lwt_io.stdin |> Lwt_stream.map f
