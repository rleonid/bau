open Bigarrayo

let time s f =
  let n = Sys.time () in
  let r = f () in
  Printf.printf "%-30s:%f\n" s (Sys.time () -. n);
  r

let generate kind n =
  let gen  = Generators.random kind in
  let native = Array.init n (fun _ -> gen ()) in
  let f = Array1.of_array kind Fortran_layout native in
  let c = Array1.of_array kind C_layout native in
  native, f, c

let sum_n (v : float array) = Array.fold_left (+.) 0. v
let sum_r v =
  let r = ref 0. in
  for i = 1 to Array1.dim v do
    r := !r +. Array1.unsafe_get v i
  done;
  !r
let sum_f v = [%array1.float64.fortran fold_left (+.) 0. v]
let sum_fl v = [%array1.float64.fortran fold_left ~init:0. ~f:(+.) v]
let sum_l v = [%array1.float64 fold_left (+.) 0. v]
let sum_ll v = [%array1.float64 fold_left ~init:0. ~f:(+.) v]

let () =
  let samples, n =
    if Array.length Sys.argv < 3 then
      10000, 40
    else
      int_of_string Sys.argv.(1)
      , int_of_string Sys.argv.(2)
  in
  Printf.printf "%d samples of %d \n" samples n;
  let data = Array.init samples (fun _ -> generate Float64 n) in
  let test name op = time name (fun () -> Array.map op data) in
  let native  = test "native" (fun (n,_,_) -> sum_n n) in
  let regular = test "regular fold" (fun (_,f,_) -> sum_r f) in
  let typed   = test "created fold_ppx" (fun (_,f,_) -> sum_f f) in
  let _       = test "created fold_ppx labeled" (fun (_,f,_) -> sum_fl f) in
  let wlayout = test "no layout" (fun (_,f,_) -> sum_l f) in
  let _       = test "no layout labeled" (fun (_,f,_) -> sum_l f) in
  Printf.printf "equal %b\n"
    (native = regular && regular = typed && typed = wlayout)
