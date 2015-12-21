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
    r := f (Array1.unsafe_get a i) !r
  done;
  !r

let a1_fold_left_safe f (i : float) a =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f (Array1.get a i) !r
  done;
  !r

let sum_n = Array.fold_left (+.) 0.
let sum_n_cons (a : float array) = Array.fold_left (+.) 0. a
let sum_f_unsafe = a1_fold_left (+.) 0.
let sum_f_safe = a1_fold_left_safe (+.) 0.

open Lacaml.D

let sum_l v = Vec.sum v

let a1_fold_left_cons f i (a : vec) =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f (Array1.unsafe_get a i) !r
  done;
  !r

let sum_f_cons = a1_fold_left_cons (+.) 0.

(* The type must be constrained fully:
  let a1_fold_left_cons2 (type l) f i (a : (float, float64_elt, l) Array1.t) =
  doesn't work.  *)
let a1_fold_left_cons2 f i (a : (float, float64_elt, fortran_layout) Array1.t) =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f (Array1.unsafe_get a i) !r
  done;
  !r

let sum_f_cons2 = a1_fold_left_cons2 (+.) 0.

let test samples n =
  let data         = Array.init samples (fun _ -> generate n) in
  let tn ()        = Array.map (fun (n, _, _) -> sum_n n) data in
  let tn_cons ()   = Array.map (fun (n, _, _) -> sum_n_cons n) data in
  let tf_unsafe () = Array.map (fun (_, f, _) -> sum_f_unsafe f) data in
  let tf_safe ()   = Array.map (fun (_, f, _) -> sum_f_safe f) data in
  let tl_lacaml () = Array.map (fun (_, f, _) -> sum_l f) data in
  let tf_cons ()   = Array.map (fun (_, f, _) -> sum_f_cons f) data in
  let tf_con2 ()   = Array.map (fun (_, f, _) -> sum_f_cons2 f) data in
  let native  = time "native" tn in
  let nat_con = time "nat cons" tn_cons in
  let unsafe  = time "unsafe" tf_unsafe in
  let safe    = time "safe" tf_safe in
  let lacaml  = time "lacaml" tl_lacaml in
  let constra = time "const" tf_cons in
  let constr2 = time "const 2" tf_con2 in
  Printf.printf "equal %b\n"
    (native = nat_con
     && nat_con = unsafe
     && unsafe = safe
     && safe = lacaml
     && lacaml = constra
     && constra = constr2
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

