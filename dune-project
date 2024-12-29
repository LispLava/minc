(lang dune 3.15)

(name minc)

(generate_opam_files true)

(source
 (github LispLava/minc))

(authors "LispLava")

(maintainers "LispLava")

(license MIT)

; (documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name minc)
 (synopsis "A minimalistic compiler for a simple C-like low level Meta Language")
 (description "Minc is a work-in-progress (WIP) minimalistic compiler for a C-like, low-level Meta Language, built with the help of Menhir and LLVM.")
 (depends ocaml dune)
 (tags
  (topics "compiler" "c-like" "low-level")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
