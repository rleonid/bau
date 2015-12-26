open Bigarray

let time s f =
  let n = Sys.time () in
  let r = f () in
  Printf.printf "%-10s:%f\n" s (Sys.time () -. n);
  r

let generate ?(u=1e3) n =
  let native =
    Array.init n (fun _ ->
      2.0 *. (Random.float u) -. u)
  in
  let fl64for = Array1.of_array Float64 Fortran_layout native in
  let fl64c = Array1.of_array Float64 C_layout native in
  native, fl64for, fl64c

let isf (type ll)(l : ll layout) =
  match l with
  | Fortran_layout -> true
  | C_layout       -> false

let to_offset l = if isf l then (fun i -> i + 1) else (fun i -> i)

let a1_fold_left f i a =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

let a1_fold_left_safe f (i : float) a =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.get a i)
  done;
  !r

let sum_n = Array.fold_left (+.) 0.
let sum_n_cons (a : float array) = Array.fold_left (+.) 0. a
(*let sum_f_unsafe = a1_fold_left (+.) 0.
let sum_f_safe = a1_fold_left_safe (+.) 0. *)

open Lacaml.D

let sum_l v = Vec.sum v

let a1_fold_left_cons f i (a : vec) =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

let sum_f_cons = a1_fold_left_cons (+.) 0.

(* The type must be constrained fully:
  let a1_fold_left_cons2 (type l) f i (a : (float, float64_elt, l) Array1.t) =
  doesn't work.  *)
let a1_fold_left_cons2 f i (a : (float, float64_elt, fortran_layout) Array1.t) =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

let sum_f_cons2 = a1_fold_left_cons2 (+.) 0.

let a1_fold_left_cons3_fortran f i (a : (float, float64_elt, fortran_layout) Array1.t) =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

let a1_fold_left_cons3_c f i (a : (float, float64_elt, c_layout) Array1.t) =
  let r = ref i in
  for i = 0 to Array1.dim a - 1 do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

(* Parameterized over layout but not kind and fast. *)
let sum_f_cons3 (type l) (v : ('a, 'b, l) Array1.t) : float =
  match Array1.layout v with
  | Fortran_layout -> a1_fold_left_cons3_fortran (+.) 0. v
  | C_layout       -> a1_fold_left_cons3_c (+.) 0. v

(* Parameterized over kind but not layout and fast. *)
let a1_fold_left_cons4_float64 f i (a : (float, float64_elt, fortran_layout) Array1.t) =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

let a1_fold_left_cons4_float32 f i (a : (float, float32_elt, fortran_layout) Array1.t) =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

let sum_f_cons4 (type a) (type b) (v : (a, b, fortran_layout) Array1.t) : float =
  match Array1.kind v with
  | Float64        -> a1_fold_left_cons4_float64 (+.) 0. v
  | Float32        -> a1_fold_left_cons4_float32 (+.) 0. v
  | _              -> failwith "NI"

(* best we can do *)
let a1_fold_left_gen (type l) f i (v : (float,float64_elt,l) Array1.t) =
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

  (*
  match Array1.layout v with
  | C_layout -> a1_fold_left_cons3_c f i v
  | Fortran_layout -> a1_fold_left_cons3_fortran f i v
    match Array1.layout v with
    | C_layout -> 0, Array1.dim v - 1
    | Fortran_layout -> 1, Array1.dim v
  in
  let r = ref i in
  for i = s to e do r := f !r (Array1.unsafe_get v i) done;
  !r
*)

let sum_f_g v = a1_fold_left_gen (+.) 0. v

let test samples n =
  let data         = Array.init samples (fun _ -> generate n) in
  let tn ()        = Array.map (fun (n, _, _) -> sum_n n) data in
  let tn_cons ()   = Array.map (fun (n, _, _) -> sum_n_cons n) data in
  (*let tf_unsafe () = Array.map (fun (_, f, _) -> sum_f_unsafe f) data in
  let tf_safe ()   = Array.map (fun (_, f, _) -> sum_f_safe f) data in
  let tl_lacaml () = Array.map (fun (_, f, _) -> sum_l f) data in
  let tf_cons ()   = Array.map (fun (_, f, _) -> sum_f_cons f) data in
  let tf_con2 ()   = Array.map (fun (_, f, _) -> sum_f_cons2 f) data in
  let tf_con3 ()   = Array.map (fun (_, _, c) -> sum_f_cons3 c) data in
  let tf_con4 ()   = Array.map (fun (_, f, _) -> sum_f_cons4 f) data in *)
  let tf_cogf ()   = Array.map (fun (_, f, _) -> sum_f_g f) data in
  let tf_cogc ()   = Array.map (fun (_, _, c) -> sum_f_g c) data in
  let native  = time "native" tn in
  let nat_con = time "nat cons" tn_cons in
  (*let unsafe  = time "unsafe" tf_unsafe in
  let safe    = time "safe" tf_safe in
  let lacaml  = time "lacaml" tl_lacaml in
  let constra = time "const" tf_cons in
  let constr2 = time "const 2" tf_con2 in
  let constr3 = time "const 3" tf_con3 in
  let constr4 = time "const 4" tf_con4 in *)
  let cons_gf = time "con gen f" tf_cogf in
  let cons_gc = time "con gen c" tf_cogc in
  Printf.printf "equal %b\n"
    (native = nat_con
     && nat_con = (*unsafe
     && unsafe = safe
     && safe = lacaml
     && lacaml = constra
     && constra = constr2
     && constr2 = constr3
     && constr3 = constr4
     && constr4 = *)cons_gf
     && cons_gf = cons_gc
    )

let () =
  if not (!Sys.interactive) then begin
    let runs, n  =
      if Array.length Sys.argv < 3 then
        10000, 40
      else
        int_of_string Sys.argv.(1)
        , int_of_string Sys.argv.(2)
    in
    Printf.printf "%d runs of %d \n" runs n;
    test runs n
  end

(* ocamlbuild -use-ocamlfind -package bigarray -package lacaml -I src/scripts/ profile.native
   Sample run:
*)

