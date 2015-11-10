
bau:
	ocamlbuild -use-ocamlfind -package bigarray -I src bau.cmo bau.cma bau.cmxa bau.cmxs

#regression test
reg:
	ocamlbuild -use-ocamlfind -package bigarray -I src/lib -I src/lib_io -I src/test regt.native

clean:
	ocamlbuild -clean

install:
	ocamlfind install bau META \
		_build/src/bau.a \
		_build/src/bau.o \
		_build/src/bau.cma \
		_build/src/bau.cmxa \
		_build/src/bau.cmxs \
		_build/src/*.cmi \
		_build/src/*.cmo \
		_build/src/*.cmx
