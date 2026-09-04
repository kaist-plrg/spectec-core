(lang dune 3.7)
(cram enable)
(using menhir 2.0)
(using mdx 0.2)
(using dune_site 0.1)
(name spectec)

(generate_opam_files true)

(package
  (name spectec)
  (version 0.0.1)
  (synopsis "SpecTecX: an executable subset of SpecTec")
  (maintainers "kunjeong99@naver.com")
  (sites
    (lib target_plugins))
  (depends
    (ocaml (>= 5.1.0))
    (menhir (and :build (>= 20240715)))
    menhirLib
    (bignum (>= 0.17.0))
    (core (>= 0.17.1))
    (core_unix (>= 0.17.0))
    dune-site
    ppx_let
    pprint
    linol-eio
    eio_main))
