
eval $(opam env)
unzip src.zip
oasis setup
echo "\"src/ltl_parser.ml\": syntax_camlp4o" >> _tags
echo "\"src/alphabet_parser.ml\": syntax_camlp4o" >> _tags
ocaml setup.ml -configure
ocaml setup.ml -build
