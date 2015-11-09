
open Bigarray

let mat_map f m = Array.map (Array.map f) m

let gen_matrix_f ?(u=1e3) n m =
  let native =
    Array.init n (fun _ ->
        Array.init m (fun _ ->
            2.0 *. (Random.float u) -. u))
  in
  let fl32for = Array2.of_array Float32 Fortran_layout native in
  let fl32c = Array2.of_array Float32 C_layout native in
  let fl64for = Array2.of_array Float64 Fortran_layout native in
  let fl64c = Array2.of_array Float64 C_layout native in
  native, fl32for, fl32c, fl64for, fl64c

let gen_matrix_is ?(u=100) n m =
  let native =
    Array.init n (fun _ ->
        Array.init m (fun _ ->
            2 * (Random.int u) - u))
  in
  let i8sf = Array2.of_array Int8_signed Fortran_layout native in
  let i8sc = Array2.of_array Int8_signed C_layout native in
  let i16sf = Array2.of_array Int16_signed Fortran_layout native in
  let i16sc = Array2.of_array Int16_signed C_layout native in
  native, i8sf, i8sc, i16sf, i16sc

let gen_matrix_i ?(u=100) n m =
  let native =
    Array.init n (fun _ ->
        Array.init m (fun _ ->
            2 * (Random.int u) - u))
  in
  let i   = Array2.of_array Int Fortran_layout native in
  let i32 = Array2.of_array Int32 Fortran_layout (mat_map Int32.of_int native) in
  let i64 = Array2.of_array Int64 Fortran_layout (mat_map Int64.of_int native) in
  let nai = Array2.of_array Nativeint Fortran_layout (mat_map Nativeint.of_int native) in
  native, i, i32, i64, nai

let gen_matrix_char n m =
  let native =
    Array.init n (fun _ ->
        Array.init m (fun _ ->
            Char.chr (Random.int 256)))
  in
  let cf = Array2.of_array Char Fortran_layout native in
  let cc = Array2.of_array Char C_layout native in
  native, cf, cc

let gen_matrix_comp ?(u=1e3) n m =
  let open Complex in
  let native =
    Array.init n (fun _ ->
        Array.init m (fun _ ->
            { re = 2.0 *. (Random.float u) -. u
            ; im = 2.0 *. (Random.float u) -. u
            }))
  in
  let c32for = Array2.of_array Complex32 Fortran_layout native in
  let c32c = Array2.of_array Complex32 C_layout native in
  let c64for = Array2.of_array Complex64 Fortran_layout native in
  let c64c = Array2.of_array Complex64 C_layout native in
  native, c32for, c32c, c64for, c64c
