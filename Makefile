# Copyright {2016} {Leonid Rozenberg}
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

LIB_EXTS=cma cmxa cmxs
INSTALL_EXTS=$(LIB_EXTS) a o cmi cmo cmx

default: bau bau_top fold_ppx

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
	ocamlfind install bau META $(foreach e,$(INSTALL_EXTS),_build/src/lib/*.$(e)) \
		_build/src/fold_ppx/fold_ppx.byte
		
profile:
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -I src/lib -I src/scripts/ profile.native

profile_int:
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -I src/scripts/ profile_int.native

profile_row_col_fold:
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -I src/scripts/ profile_row_col_fold.native

profile_fold:
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -I src/scripts/ -I src/lib profile_fold.native

fold_ppx:
	ocamlbuild -use-ocamlfind -pkgs compiler-libs.common -I +compiler-libs -I src/fold_ppx fold_ppx.byte

fold_ppx_test: fold_ppx bau
	ocamlbuild -use-ocamlfind -pkgs bigarray -tags "ppx(src/fold_ppx/fold_ppx.byte)" -I src/lib -I src/fold_ppx -I src/scripts -cflag -dsource fold_ppx_test.native

fold_ppx_prof: fold_ppx bau
	ocamlbuild -use-ocamlfind -pkgs bigarray -tags "ppx(src/fold_ppx/fold_ppx.byte)" -I src/lib -I src/fold_ppx -I src/scripts -cflag -dsource fold_ppx_prof.native
