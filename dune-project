(lang dune 2.7)

(name sql_query)
(version v0.0.1)
(generate_opam_files true)
(license GPL-3.0)
(authors "Yawar Amin <yawar.amin@gmail.com>")
(maintainers "Yawar Amin <yawar.amin@gmail.com>")
(source (github yawaramin/ocaml_sql_query))

(package
  (name sql_query)
  (synopsis "TBD")
  (description "TBD")
  (documentation "TBD")
  (depends
    (ocaml (>= 4.08.0))
    (sqlite3 (and (>= 5.1.0) (< 6.0.0)))))
