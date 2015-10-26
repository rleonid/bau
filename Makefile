
bau:
	ocamlbuild -use-ocamlfind -package bigarray -I src/lib bau.cma

#regression test
reg:
	ocamlbuild -use-ocamlfind -package bigarray -I src/lib -I src/lib_io -I src/test regt.native

clean:
	ocamlbuild -clean
