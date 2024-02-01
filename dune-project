(lang dune 3.12)

(name ocaml_aoc_2023)

(generate_opam_files true)

(source
 (github caasi/ocaml_aoc_2023))

(authors "Isaac Huang")

(maintainers "Isaac Huang")

(license LICENSE)

(package
 (name ocaml_aoc_2023)
 (synopsis "Advent of Code 2023")
 (description "Solving Advent of Code 2023 in OCaml")
 (depends ocaml dune lwt_ppx angstrom)
 (tags
  ("Advent of Code" aoc)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
