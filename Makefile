
LIB_EXTS=cma cmxa cmxs
INSTALL_EXTS=$(LIB_EXTS) a o cmi cmo cmx so

#	ocamlbuild -use-ocamlfind -pkg bigarray -I src bau.cmo bau.cma bau.cmxa bau.cmxs

bau:
	ocamlbuild -use-ocamlfind -pkg bigarray -I src/lib $(foreach e,$(LIB_EXTS),bau.$(e)) 

bau_top:
	ocamlbuild -use-ocamlfind -pkg bigarray -pkg compiler-libs -I src/lib $(foreach e,$(LIB_EXTS),bau_top.$(e))
	
#ocamlbuild -use-ocamlfind -pkg bigarray -I src -tag-line '<src/*/*.cmx> and not <src/bau_top.cm*> : for-pack(Bautop)' $(foreach e,$(LIB_EXTS),bautop.$(e))

#regression test
reg:
	ocamlbuild -use-ocamlfind -pkg bigarray -I src/lib -I src/test $(foreach e,$(LIB_EXTS),bau.$(e)) regt.native

clean:
	ocamlbuild -clean

install:
	ocamlfind install bau META $(foreach e,$(INSTALL_EXTS),_build/src/lib/*.$(e))
		
profile:
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -I src/lib -I src/scripts/ profile.native

profile_int:
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -I src/scripts/ profile_int.native

profile_row:
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -I src/scripts/ profile_row_col_fold.native

profile_fold:
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -I src/scripts/ -I src/lib profile_fold.native

fold_ppx:
	ocamlbuild -use-ocamlfind -pkgs compiler-libs.common -I +compiler-libs -I src/fold_ppx fold_ppx.byte

fold_ppx_test: fold_ppx
	ocamlbuild -use-ocamlfind -pkgs bigarray -tags "ppx(src/fold_ppx/fold_ppx.byte)" -I src/fold_ppx -cflag -dsource fold_ppx_test.native
