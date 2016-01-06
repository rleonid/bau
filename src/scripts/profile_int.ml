open Bigarray

let time s f =
  let n = Sys.time () in
  let r = f () in
  Printf.printf "%-10s:%f\n" s (Sys.time () -. n);
  r

let generate ?(u=100) n =
  let native =
    Array.init n (fun _ ->
      2 * (Random.int u) - u)
  in
  let f = Array1.of_array Int Fortran_layout native in
  let c = Array1.of_array Int C_layout native in
  native, f, c

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

let a1_fold_left_safe f (i : int) a =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.get a i)
  done;
  !r

let sum_n = Array.fold_left (+) 0
let sum_n_cons (a : int array) = Array.fold_left (+) 0 a
let sum_f_unsafe = a1_fold_left (+) 0
let sum_f_safe = a1_fold_left_safe (+) 0

let a1_fold_left_f f i (a : (int, int_elt, fortran_layout) Array1.t) =
  let r = ref i in
  for i = 1 to Array1.dim a do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

let a1_fold_left_c f i (a : (int, int_elt, c_layout) Array1.t) =
  let r = ref i in
  for i = 0 to Array1.dim a - 1 do
    r := f !r (Array1.unsafe_get a i)
  done;
  !r

(* best we can do *)
let a1_fold_left_gen (type l) f i (v : (int,int_elt,l) Array1.t) =
  match Array1.layout v with
  | C_layout       -> a1_fold_left_c f i v
  | Fortran_layout -> a1_fold_left_f f i v

let sum_f_g v = a1_fold_left_gen (+) 0 v

let test samples n =
  let data         = Array.init samples (fun _ -> generate n) in
  let tn ()        = Array.map (fun (n, _, _) -> sum_n n) data in
  let tn_cons ()   = Array.map (fun (n, _, _) -> sum_n_cons n) data in
  let tf_unsafe () = Array.map (fun (_, f, _) -> sum_f_unsafe f) data in
  let tf_safe ()   = Array.map (fun (_, f, _) -> sum_f_safe f) data in
  let tf_cogf ()   = Array.map (fun (_, f, _) -> sum_f_g f) data in
  let tf_cogc ()   = Array.map (fun (_, _, c) -> sum_f_g c) data in
  let native  = time "native" tn in
  let nat_con = time "nat cons" tn_cons in
  let unsafe  = time "unsafe" tf_unsafe in
  let safe    = time "safe" tf_safe in
  let cons_gf = time "con gen f" tf_cogf in
  let cons_gc = time "con gen c" tf_cogc in
  Printf.printf "equal %b\n"
    (native = nat_con
     && nat_con = unsafe
     && unsafe = safe
     && safe = cons_gf
     && cons_gf = cons_gc)

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

