(lang dune 2.7)

(name fun_sql)

(version v0.1.0)

(generate_opam_files true)

(license GPL-3.0)

(authors "Yawar Amin <yawar.amin@gmail.com>")

(maintainers "Yawar Amin <yawar.amin@gmail.com>")

(source
 (github yawaramin/ocaml_sql_query))

(package
 (name fun_sqlite)
 (synopsis "TBD")
 (description "TBD")
 (documentation "TBD")
 (tags (org:yawaramin))
 (depends
  (sqlite3
   (and
    (>= 5.1.0)
    (< 6.0.0)))))

(package
 (name fun_postgresql)
 (synopsis "TBD")
 (description "TBD")
 (documentation "TBD")
 (tags (org:yawaramin))
 (depends
  (postgresql
   (and
    (>= 5.0.0)
    (< 6.0.0)))))
