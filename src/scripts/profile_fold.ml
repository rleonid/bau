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
  let general =
    native
    |> Array.to_list
    |> Array.concat
    |> GA.of_array ~dims:[|n;m|] Float64 Fortran_layout
  in
  native, general

let sum_native arr =
  let n = Array.length arr in
  let m = Array.length arr.(0) in
  let s = ref 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      s := !s +. arr.(i).(j)
    done
  done;
  !s

(* we have 2d's in Fortran style *)
let sum_general_naive ga = 
  let [| n; m |] = GA.dims ga in
  let s = ref 0.0 in
  for i = 1 to n do
    for j = 1 to m do
      s := !s +. GA.get ga [| i; j |]
    done
  done;
  !s

let sum_general_fold = fold (+.) 0.0

open Lacaml.D

let sum_lacaml = Mat.sum

let d = 1e-5
let significantly_different_from x y = y < (x -. d) || y > (x +. d )
let equal_floats x y = not (significantly_different_from x y)

let eq_array arr1 arr2 =
  Array.fold_left (fun (b, i) v ->
    b && equal_floats v arr2.(i), i + 1) (true, 0) arr1
  |> fst


let test samples n m =
  let data = Array.init samples (fun _ -> generate n m) in
  let as_m = Array.map (fun (_, g) -> array2_of_genarray g) data in
  let tna () = Array.map (fun (n, _) -> sum_native n) data in
  let tgn () = Array.map (fun (_, g) -> sum_general_naive g) data in
  let tgf () = Array.map (fun (_, g) -> sum_general_fold g) data in
  let tla () = Array.map sum_lacaml as_m in
  let rna = time "native" tna in
  let rgn = time "g naive" tgn in
  let rgf = time "g fold" tgf in
  let rla = time "lacaml" tla in
  (* rna, rgn, rgf, (rla : float array) *)
  eq_array rna rgn &&
  eq_array rgn rgf &&
  eq_array rgf rla


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
    Printf.printf "equal results: %b\n" (test runs n m)
  end

(* ocamlbuild -use-ocamlfind -package bigarray -pacakage lacaml -I src/scripts/ profile_fold.native
  Sample run:

  $ ./profile_fold.native 
  10000 runs of 40 by 50
  native    :0.065925
  g naive   :0.323224
  g fold    :0.220276
  lacaml    :0.021637
  equal results: true

  $ ./profile_fold.native 
  10000 runs of 40 by 50
  native    :0.065897
  g naive   :0.311273
  g fold    :0.212764
  lacaml    :0.019933
  equal results: true

  $ ./profile_fold.native 1000 400 500
  1000 runs of 400 by 500
  native    :0.663819
  g naive   :4.471635
  g fold    :2.382543
  lacaml    :0.225448
  equal results: true
   
  Faster but not fast enough ?
*)
