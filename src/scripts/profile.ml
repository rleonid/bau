open Bigarray

let time s f =
  let n = Sys.time () in
  let r = f () in
  Printf.printf "%-10s:%f\n" s (Sys.time () -. n);
  r

let generate ?(u=1e3) n m =
  let native =
    Array.init n (fun _ ->
        Array.init m (fun _ ->
            2.0 *. (Random.float u) -. u))
  in
  let fl64for = Array2.of_array Float64 Fortran_layout native in
  let fl64c = Array2.of_array Float64 C_layout native in
  native, fl64for, fl64c

let isf (type ll)(l : ll layout) =
  match l with
  | Fortran_layout -> true
  | C_layout       -> false

let to_offset l = if isf l then (fun i -> i + 1) else (fun i -> i)

let foreach l n f =
  if isf l then
    for i = 1 to n do f i done
  else
    for i = 0 to n - 1 do f i done

module A2 = struct
  include Array2

  let row_folds a f i =
    let l = layout a in
    let n = dim2 a in
    let v = ref i in
    let o = to_offset l in
    Array.init (dim1 a) (fun r ->
        v := i;
        foreach l n (fun c -> v := f !v (unsafe_get a (o r) c));
        !v)

  let col_folds a f i =
    let l = layout a in
    let n = dim1 a in
    let v = ref i in
    let o = to_offset l in
    Array.init (dim2 a) (fun c ->
      v := i;
      foreach l n (fun r -> v := f !v (unsafe_get a r (o c)));
      !v)

end

let sum_rows_n = Array.map (Array.fold_left (+.) 0.)

let sum_rows_b m = A2.row_folds m (+.) 0.0

let sum_cols_n m =
  let r = Array.make (Array.length m.(0)) 0.0 in
  Array.iter (Array.iteri (fun i v -> r.(i) <- r.(i) +. v)) m;
  r

let sum_cols_b m = A2.col_folds m (+.) 0.0

open Lacaml.D

let sum_cols_l m =
  Array.init (A2.dim2 m) (fun i ->
    let s = A2.slice_right m (i + 1) in
    Vec.sum s)

let test samples n m =
  let data = Array.init samples (fun _ -> generate n m) in
  let tn () = ignore (Array.map (fun (n, _, _) -> ignore (sum_rows_n n)) data) in
  let tf () = ignore (Array.map (fun (_, f, _) -> ignore (sum_rows_b f)) data) in
  let tc () = ignore (Array.map (fun (_, _, c) -> ignore (sum_rows_b c)) data) in
  time "native" tn;
  time "fortran" tf;
  time "c" tc;
  let tn () = ignore (Array.map (fun (n, _, _) -> ignore (sum_cols_n n)) data) in
  let tf () = ignore (Array.map (fun (_, f, _) -> ignore (sum_cols_b f)) data) in
  let tc () = ignore (Array.map (fun (_, _, c) -> ignore (sum_cols_b c)) data) in
  let tl () = ignore (Array.map (fun (_, f, _) -> ignore (sum_cols_l f)) data) in
  time "native" tn;
  time "fortran" tf;
  time "c" tc;
  time "lacaml" tl

let () =
  if not (!Sys.interactive) then begin
    let runs, n, m =
      if Array.length Sys.argv < 4 then
        10000, 40, 50
      else
        int_of_string Sys.argv.(1)
        , int_of_string Sys.argv.(2)
        , int_of_string Sys.argv.(3)
    in
    Printf.printf "%d runs of %d by %d\n" runs n m;
    test runs n m
  end

(* ocamlbuild -use-ocamlfind -package bigarray -pacakage lacaml -I src/scripts/ profile.native
   Sample run:
$ ./profile.native 10 3000 3000
native    :0.407063
fortran   :1.794288
c         :3.703106
native    :0.312351
fortran   :3.426450
c         :1.829178

With Lacaml: ./profile.native 100 300 300
native    :0.036494
fortran   :0.289516
c         :0.185806
native    :0.044074
fortran   :0.205087
c         :0.266259
lacaml    :0.016714  <--- best!

./profile.native 10 3000 3000
native    :0.377344
fortran   :4.466229
c         :2.012626
native    :0.473492
fortran   :2.073251
c         :3.913682
lacaml    :0.100399  <--- best!
*)

