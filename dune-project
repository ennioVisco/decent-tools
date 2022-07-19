(lang dune 3.3)

(name decent)

(generate_opam_files true)

(source
 (uri git+https://gitlab.inria.fr/eviscont/decent.git))

(authors "Ylies Falcone" "Florian Gallay" "Ennio Visconti")

(maintainers "Florian Gallay")

(license LICENSE)

(documentation https://gitlab.inria.fr/monitoring/decent)


; TODO: set a local opam switch

(package
 (name decent)
 (synopsis "An OCaml Benchmark for Decentralised Enforcement of LTL Formulae")
 (description "A longer description")
 (depends ocaml dune batteries camlp5 ocamlbuild ocamlfind)
 (tags
  (monitoring enforcement "decentralized analysis" LTL)))
  
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
