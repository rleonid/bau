
set -e

${HOME}/opam pin add -k git bigarray_fold_ppx https://github.com/rleonid/bigarray_fold_ppx.git

echo Compiling
make
