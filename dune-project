(lang dune 3.0)
(using menhir 2.0)
(name p4cherry)

(generate_opam_files true)

(package
  (name p4cherry)
  (version 0.0.1)
  (synopsis "P4Cherry: A to-be interpreter of the P4 Programming Language")
  (maintainers "99jaehyunlee@gmail.com")
  (depends
    bignum
    (core (and (>= "v0.15.0") (< "v0.16.0")))))
