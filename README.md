[![Build Status](https://travis-ci.org/rleonid/bau.svg?branch=master)](https://travis-ci.org/rleonid/bau)

BigArray Utilities
------------------

Some routines (a hodge-podge) to make it easier to work with
[OCaml Bigarrays](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.html).

### Pretty printing

```OCaml
# let n, f, c = gen3d 3 4 5  ;;
val n : float array array array =
  [|[|[|963.705; 182.747; -568.959; 643.522; 697.603|]; [|-706.857; 86.036; 27.626; -874.151; 559.791|];
      [|-263.671; -392.582; 268.487; -488.787; 55.409|]; [|-988.593; 52.948; -817.274; -785.550; -43.221|]|];
    [|[|226.351; 505.945; 370.838; -734.224; -634.506|]; [|304.503; -330.526; -800.156; -570.671; -514.998|];
      [|-470.820; 731.985; -501.758; -856.375; 422.431|]; [|819.669; 847.078; 301.306; -917.181; 391.153|]|];
    [|[|-936.169; 855.439; 730.839; 763.087; 777.606|]; [|945.573; -584.689; 312.286; 840.451; -84.875|];
      [|-124.934; 980.958; 383.844; -290.605; 53.375|]; [|-133.827; 509.676; 211.333; 866.180; -767.171|]|]|]
  ...
# f ;;
val f : (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array3.t =
         C1       C2       C3       C4       C1       C2       C3      C4        C1       C2       C3       C4
R1  963.705 -706.857 -263.671 -988.593  182.747   86.036 -392.582 52.9484  -568.959  27.6259  268.487 -817.274
R2  226.351  304.503  -470.82  819.669; 505.945 -330.526  731.985 847.078;  370.838 -800.156 -501.758  301.306;
R3 -936.169  945.573 -124.934 -133.827  855.439 -584.689  980.958 509.676   730.839  312.286  383.844  211.333
         C1       C2       C3       C4        C1       C2      C3       C4
R1  643.522 -874.151 -488.787  -785.55   697.603  559.791  55.409 -43.2214
R2 -734.224 -570.671 -856.375 -917.181; -634.506 -514.998 422.431  391.153;
R3  763.087  840.451 -290.605   866.18   777.606 -84.8751  53.375 -767.171
# c ;;
val c : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array3.t =
         C0       C1       C2       C3       C4       C0       C1       C2       C3       C4
R0  963.705  182.747 -568.959  643.522  697.603  226.351  505.945  370.838 -734.224 -634.506
R1 -706.857   86.036  27.6259 -874.151  559.791; 304.503 -330.526 -800.156 -570.671 -514.998;
R2 -263.671 -392.582  268.487 -488.787   55.409  -470.82  731.985 -501.758 -856.375  422.431
R3 -988.593  52.9484 -817.274  -785.55 -43.2214  819.669  847.078  301.306 -917.181  391.153
         C0       C1      C2       C3       C4
R0 -936.169  855.439 730.839  763.087  777.606
R1  945.573 -584.689 312.286  840.451 -84.8751;
R2 -124.934  980.958 383.844 -290.605   53.375
R3 -133.827  509.676 211.333   866.18 -767.171
```

### Folds and Iter via PPX

#### Motivation

The `OCaml` compiler has built in primitives (see `caml_ba_ref_` in
`bigarray.mli`) that can be used to index into a Bigarray faster if the full
[kind](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.html#TYPEkind)
of the bigarray is known. To avoid repeated writing of the type signatures we use
`ppx` to write an efficient `fold_left`, `fold_right` or `iter`:

```OCaml
let sum_b v = [%array1.fold_left.float64 (+.) 0. v]
```

A simple profiling program: [`fold_ppx_prof.ml`](src/scripts/fold_ppx_prof.ml)
compares three implementations of summing either a native array of floats or
an `(float, float64, fortran_layout) Array1.t` :

1. Using native arrays.

  ```OCaml
  let sum_n (v : float array) = Array.fold_left (+.) 0. v
  ```

2. Without specifying the type of the `Array1.t`

  ```OCaml
  let sum_r v =
    let r = ref 0. in
    for i = 1 to Array1.dim v do
      r := !r +. Array1.unsafe_get v i
    done;
    !r
  ```

3. Using the typed code generated via `fold_ppx`:

  ```OCaml
  let sum_f v = [%array1.float64.fortran fold_left (+.) 0. v]
  ```

Typical performance results look like:

  ```bash
  $ ./fold_ppx_prof.native
  10000 samples of 40
  native                        :0.001807
  regular fold                  :0.005426
  created fold_ppx              :0.001455
  ```

#### Usage

The general syntax is

```OCaml
[%bigarraytype.kind(.layout) operation f (init) v]
```

  - `bigarraytype` - Currently only supports `"array1"`
  - `kind` - One of:
          `"float32"`,
          `"float64"`,
          `"complex32"`,
          `"complex64"`,
          `"int8_signed"`,
          `"int8_unsigned"`,
          `"int16_signed"`,
          `"int16_unsigned"`,
          `"int32"`,
          `"int64"`,
          `"int"`,
          `"nativeint"`,
          `"char"`.
  - `layout` Optional but can be `"fortran"` or `"c"`. If left off `fold_ppx`
    will generate code that detects the layout and acts accordingly.
  - `operation` - `"fold_left"`, `"fold_right"` or `"iter"`

  Arguments:
  - `f` the `fold` or `iter` function to apply. If `v` has type
    `(k,'b, 'c) Array1.t` then `f` should have types:
      - `fold_left`  : `('a -> k -> 'a)`
      - `fold_right` : `(k -> 'a -> 'a)` 
      - `iter`       : `(k -> unit)`

    Just like regular `Array` values. This can be applied with a label: `~f`
    but then `~init` must be labeled as well (for fold only).

  - `init` the initial value, only for folds
  - `v` the `Array1`

### Random generators

[Generators](src/lib/generators.mli) to make it easier to create test Bigarrays.
