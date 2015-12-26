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

(*
let sum_n (v : float array) = Array.fold_left (+.) 0. v
let sum_b v = [%array1.fold_left.float64 (+.) 0. v]
   *)

let () =
  let v = Array1.of_array Float64 Fortran_layout [|1.;2.;3.|] in
  let add2 y x = x +. y in
  let four = 4. in
  if !Sys.interactive then () else
    Printf.printf "%f\n" [%test];
    Printf.printf "%f\n" [%test add2 four v];

(*
  let samples, n =
    if Array.length Sys.argv < 3 then
      10000, 40
    else
      int_of_string Sys.argv.(1)
      , int_of_string Sys.argv.(2)
  in
  Printf.printf "%d samples of %d \n" samples n;
  let data = Array.init samples (fun _ -> generate n) in
  let test name op = time name (fun () -> Array.map op data) in
  let native = test "native" (fun (n,_,_) -> sum_n n) in
  let bgfolf = time "array1 fold fortran layout" (fun (_,f,_) -> sum_b f) in
  let bgfolc = time "array1 fold c layout" (fun (_,_,c) -> sum_b c) in
  Printf.printf "equal %b\n" (native = bgfolf && bgfolf = bgfolc)
   *)
