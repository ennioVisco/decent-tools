install:
	opam install --unlock-base oasis camlp4 batteries ocamlbuild ocamlfind
	oasis setup
	echo "\"src/ltl_parser.ml\": syntax_camlp4o" >> _tags
	echo "\"src/alphabet_parser.ml\": syntax_camlp4o" >> _tags
	ocaml setup.ml -configure
	ocaml setup.ml -build

decent:
	ocaml setup.ml -build

uninstall:
	rm -f setup.data
	rm -f setup.ml
	rm -f setup.log
	rm -f myocamlbuild.ml
	rm -rf _build
	rm -f decent.native
	rm -f _tags
	
clean:
	ocaml setup.ml -clean
