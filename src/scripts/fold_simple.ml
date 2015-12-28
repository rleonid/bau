
open Bigarray

let fold_left (type l) f i (v : (float,float64_elt,l) Array1.t) =
  let fold_left_fortran (a : (float,float64_elt,fortran_layout) Array1.t) =
    let r = ref i in
    for i = 1 to Array1.dim a do
      r := f !r (Array1.unsafe_get a i)
    done;
    !r
  in
  let fold_left_c (a : (float, float64_elt, c_layout) Array1.t) =
    let r = ref i in
    for i = 0 to Array1.dim a - 1 do
      r := f !r (Array1.unsafe_get a i)
    done;
    !r
  in
  match Array1.layout v with
  | Fortran_layout -> fold_left_fortran v
  | C_layout       -> fold_left_c v
