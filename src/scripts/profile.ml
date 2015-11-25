open Bigarrayo

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

let sum_rows_n = Array.map (Array.fold_left (+.) 0.)

let sum_rows_f m = A2.row_folds m (+.) 0.0

let sum_rows_c m = A2.row_folds m (+.) 0.0

let test samples n m =
  let data = Array.init samples (fun _ -> generate n m) in
  let tn () = ignore (Array.map (fun (n, _, _) -> ignore (sum_rows_n n)) data) in
  let tf () = ignore (Array.map (fun (_, f, _) -> ignore (sum_rows_f f)) data) in
  let tc () = ignore (Array.map (fun (_, _, c) -> ignore (sum_rows_c c)) data) in
  time "native" tn; 
  time "fortran" tc; 
  time "c" tf 

(* ocamlbuild -use-ocamlfind -package bigarray -I src/scripts/ -I src/lib/ bau.cma profile.native *)
let () =
  if not (!Sys.interactive) then
    test (int_of_string Sys.argv.(1))
         (int_of_string Sys.argv.(2))
         (int_of_string Sys.argv.(3))
