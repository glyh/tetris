(lang dune 3.17)

(name tetris)

(generate_opam_files true)

(source
 (github glyh/tetris))

(authors "Corvo Liu <lyhokia@gmail.com>")

(maintainers "Corvo Liu <lyhokia@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tetris)
 (synopsis "Simple tetris implemented in OCaml")
 (description "Simple tetris implemented in OCaml")
 (depends 
   ocaml
   (minttea (>= 0.0.3))
   (spices (>= 0.0.2))
   (riot (>= 0.0.5))
   (batteries (>= 3.8.0))
   (ppx_deriving (>= 6.0.3)))
          
 (tags
  (game)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
