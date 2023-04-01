(lang dune 3.7)

(name marching_squares)

(generate_opam_files true)

(source
 (github S-c-r-a-t-c-h-y/marching_squares))

(authors "Amaury Mazoyer")

(maintainers "Amaury Mazoyer")

(license LICENSE)

(documentation https://github.com/S-c-r-a-t-c-h-y/marching_squares/wiki)

(package
 (name marching_squares)
 (synopsis "Marching squares implementation")
 (description "A full implementation of the marching squares algorithm in OCaml")
 (depends ocaml dune)
 (tags
  (algorithm "marching squares")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
