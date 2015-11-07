
open Bigarray

let gen_matrix ?(u=1e3) n m =
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


let int_native =
  [|
    [|11; 12; 13; 14|];
    [|21; 22; 23; 24|];
    [|31; 32; 33; 34|];
  |]

let mat_map f m = Array.map (Array.map f) m
let float_native = mat_map float int_native
 

