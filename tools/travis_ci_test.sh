
eval `opam config env`
opam install -y ocamlbuild core_bench

echo Compiling
make

echo Testing
make regt fold_ppx_test

./regt.native
./fold_ppx_test.native
