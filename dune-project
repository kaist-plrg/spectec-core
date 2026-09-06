(lang dune 3.7)
(cram enable)
(using menhir 2.0)
(using mdx 0.2)
(using dune_site 0.1)
(name spectec)
(version 0.0.1)
(maintainers "kunjeong99@naver.com")

(generate_opam_files true)

(package
  (name spectec)
  (synopsis "SpecTecX: an executable subset of SpecTec")
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

(package
  (name spectec-target-impty)
  (synopsis "Impty target for SpecTecX")
  (sites
    (share specs))
  (depends
    (ocaml (>= 5.1.0))
    (spectec (= :version))
    (core (>= 0.17.1))
    dune-site
    ppx_let))

(package
  (name spectec-target-miniml)
  (synopsis "Mini-ML target for SpecTecX")
  (sites
    (share specs))
  (depends
    (ocaml (>= 5.1.0))
    (menhir (and :build (>= 20240715)))
    (spectec (= :version))
    (core (>= 0.17.1))
    dune-site
    ppx_let))

(package
  (name spectec-target-p4)
  (synopsis "P4 target for SpecTecX")
  (sites
    (share specs)
    (share old_specs))
  (depends
    (ocaml (>= 5.1.0))
    (menhir (and :build (>= 20240715)))
    menhirLib
    (spectec (= :version))
    (core (>= 0.17.1))
    (core_unix (>= 0.17.0))
    dune-site
    ppx_let))
