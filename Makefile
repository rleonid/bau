
LIB_EXTS=cmo cma cmxa cmxs
INSTALL_EXTS=$(LIB_EXTS) a o cmi cmo cmx

#	ocamlbuild -use-ocamlfind -pkg bigarray -I src bau.cmo bau.cma bau.cmxa bau.cmxs

bau:
	ocamlbuild -use-ocamlfind -pkg bigarray -I src $(foreach e,$(LIB_EXTS),bau.$(e))

bautop:
	ocamlbuild -use-ocamlfind -pkg bigarray -pkg compiler-libs -I src $(foreach e,$(LIB_EXTS),bautop.$(e))

#regression test
reg:
	ocamlbuild -use-ocamlfind -pkg bigarray -I src -I src/test $(foreach e,$(LIB_EXTS),bau.$(e)) regt.native

clean:
	ocamlbuild -clean

install:
	ocamlfind install bau META $(foreach e,$(INSTALL_EXTS),_build/src/bau.$(e))

#	ocamlfind install bau META \ _build/src/bau.a \ _build/src/bau.o \ _build/src/bau.cma \ _build/src/bau.cmxa \ _build/src/bau.cmxs \ _build/src/*.cmi \ _build/src/*.cmo \ _build/src/*.cmx
