(lang dune 3.14)

(name mycofetch)

(generate_opam_files true)

(source
 (github orest58008/mycofetch))

(authors "Orest I")

; (maintainers "Maintainer Name")

(license BSD-2-Clause)

; (documentation https://url/to/documentation)

(package
 (name mycofetch)
 (synopsis "Flexible system info tool written in OCaml")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
