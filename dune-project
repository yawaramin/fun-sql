(lang dune 2.7)

(name fun-sql)

(version v0.2.0)

(generate_opam_files true)

(license GPL-3.0)

(authors "Yawar Amin <yawar.amin@gmail.com>")

(maintainers "Yawar Amin <yawar.amin@gmail.com>")

(source
 (github yawaramin/ocaml_sql_query))

(package
 (name fun-sql)
 (synopsis "TBD")
 (description "TBD")
 (documentation "TBD")
 (tags (org:yawaramin)))

(package
 (name fun-sqlite)
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
 (name fun-postgresql)
 (synopsis "TBD")
 (description "TBD")
 (documentation "TBD")
 (tags (org:yawaramin))
 (depends
  (postgresql
   (and
    (>= 5.0.0)
    (< 6.0.0)))))
