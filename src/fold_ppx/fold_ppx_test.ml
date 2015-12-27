open Bigarray

let time s f =
  let n = Sys.time () in
  let r = f () in
  Printf.printf "%-30s:%f\n" s (Sys.time () -. n);
  r

let generate ?(u=1e3) n =
  let native =
    Array.init n (fun _ ->
      2. *. (Random.float u) -. u)
  in
  let f = Array1.of_array Float64 Fortran_layout native in
  let c = Array1.of_array Float64 C_layout native in
  native, f, c

let sum_n (v : float array) = Array.fold_left (+.) 0. v
let sum_b v = [%array1.fold_left.float64 (+.) 0. v]
let sum_b_f v = [%array1.fold_left.float64.fortran (+.) 0. v]
let sum_b_c v = [%array1.fold_left.float64.c (+.) 0. v]

let () =
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
  let native  = test "native" (fun (n,_,_) -> sum_n n) in
  let bgfolf  = test "array1 fold_left fortran" (fun (_,f,_) -> sum_b f) in
  let bgfolf2 = test "array1 fold_left fortran spec" (fun (_,f,_) -> sum_b_f f) in
  let bgfolc  = test "array1 fold c layout" (fun (_,_,c) -> sum_b c) in
  let bgfolc2 = test "array1 fold c layout spec" (fun (_,_,c) -> sum_b_c c) in
  Printf.printf "equal %b\n"
    (native = bgfolf
    && bgfolf = bgfolf2
    && bgfolf2 = bgfolc
    && bgfolc = bgfolc2)
