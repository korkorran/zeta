(lang dune 3.17)

(name zeta)

(generate_opam_files true)

(source
 (github korkorran/zeta))

(authors "Frédéric Lang <frederic.ln.lang@gmail.com>")

(maintainers "Frédéric Lang <frederic.ln.lang@gmail.com>")

(license AGPL-3.0-or-later)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name zeta)
 (allow_empty)
 (synopsis "A format and a algorithm to parse the command line")
 (description "A format describing the syntax of terminal commands, and an algorithm `Zeta` to evaluate a terminal entry to a formatted representation of the entry. The evaluation is done against a collection of description of command saved in a registry. The project aims to be a general approach to parse terminal entries despites the variety of terminal commands syntax.")
 (depends ocaml cmdliner lambda-term lwt (alcotest :with-test))
 (tags
  ("command" "terminal" "parser" "format" "ocaml")))

