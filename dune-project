(lang dune 3.17)

(name open-command)

(generate_opam_files true)

(source
 (github korkorran/open-command))

(authors "Frédéric Lang <frederic.ln.lang@gmail.com>")

(maintainers "Frédéric Lang <frederic.ln.lang@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name open-command)
 (allow_empty)
 (synopsis "A format and a algorithm to parse the command line")
 (description "A longer description")
 (depends ocaml cmdliner lambda-term lwt (alcotest :with-test))
 (tags
  ("command" "terminal" "parser" "format" "ocaml")))

