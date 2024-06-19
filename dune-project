(lang dune 2.7)

(name fun-sql)

(version v0.3.0)

(generate_opam_files true)

(license GPL-3.0-or-later)

(authors "Yawar Amin <yawar.amin@gmail.com>")

(maintainers "Yawar Amin <yawar.amin@gmail.com>")

(source
 (github yawaramin/fun-sql))

(package
 (name fun-sql)
 (synopsis "Shared code for SQL libraries")
 (description
  "Shared code for SQL packages, use fun-sqlite or fun-postgresql directly depending on which database you are using.")
 (documentation https://yawaramin.github.io/fun-sql/fun-sql/Fun_sql/)
 (tags
  (org:yawaramin fun-sql))
 (depends
  (ocaml
   (>= 4.14.0))))

(package
 (name fun-sqlite)
 (synopsis "SQLite query support")
 (description "Use this package for running SQLite queries.")
 (documentation https://yawaramin.github.io/fun-sql/fun-sqlite/Fun_sqlite/)
 (tags
  (org:yawaramin fun-sql))
 (depends
  (fun-sql
   (= :version))
  (sqlite3
   (and
    (>= 5.1.0)
    (< 6.0.0)))))

(package
 (name fun-postgresql)
 (synopsis "PostgreSQL query support")
 (description "Use this package for running PostgreSQL queries.")
 (documentation
  https://yawaramin.github.io/fun-sql/fun-postgresql/Fun_postgresql/)
 (tags
  (org:yawaramin fun-sql))
 (depends
  (fun-sql
   (= :version))
  (postgresql
   (and
    (>= 5.0.0)
    (< 6.0.0)))))

(package
 (name ppx_deriving_funsql)
 (synopsis "PPX deriver for query row decoding into OCaml types")
 (description
  "This is a convenience PPX that takes care of the boilerplate of writing the conversion from query result rows to OCaml types.")
 (documentation https://yawaramin.github.io/fun-sql/)
 (tags
  (org:yawaramin fun-sql))
 (depends
  (ppxlib
   (and
    (>= 0.32.1)
    (< 1.0.0)))))
