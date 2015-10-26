
#regression test

reg:
	ocamlbuild -use-ocamlfind -package lacaml -I src/test regt.native
