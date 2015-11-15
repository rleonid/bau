
open Bigarray

let gen3d ?(u=1e3) n m l =
  let native =
    Array.init n (fun _ ->
      Array.init m (fun _ ->
        Array.init l (fun _ ->
          2.0 *. (Random.float u) -. u)))
  in
  let fl64for = Array3.of_array Float64 Fortran_layout native in
  let fl64c   = Array3.of_array Float64 C_layout native in
  native, fl64for, fl64c



open Lacaml_io
let n, f, c = gen3d 3 4 5 ;;
let sl1 = ThreeD.slices_indices f ;;
let m1 = ThreeD.cs sl1.(0) f ;;
let b1 = Toplevel.(wrap pp_fmat) m1 ;;
let s1 = Buffer.contents b1 ;;
let tp () = ThreeD.gen_pp_3d Toplevel.(wrap pp_fmat) Format.std_formatter 70 f ;;
