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
let sum_f = a1_fold_left (+.) 0.
let sum_f_safe = a1_fold_left_safe (+.) 0.

open Lacaml.D

let sum_l v = Vec.sum v

let test samples n =
  let data = Array.init samples (fun _ -> generate n) in
  let tn () = Array.map (fun (n, _, _) -> sum_n n) data in
  let tf () = Array.map (fun (_, f, _) -> sum_f f) data in
  let tf_safe () = Array.map (fun (_, f, _) -> sum_f_safe f) data in
  let tl () = Array.map (fun (_, f, _) -> sum_l f) data in
  let nn = time "native" tn in
  let nf = time "fortran" tf in
  let nf_safe = time "safe" tf_safe in
  let nl = time "lacaml" tl in
  Printf.printf "equal %b\n"
    (nn = nf && nf = nf_safe && nf = nl)

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

