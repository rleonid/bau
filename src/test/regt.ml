

let test_size = 10
let max_matrix_size = 10

(*
let double_mat n m =
  let m = Array2.create Float64 Fortran_layout n m in
  for i = 1 
   *)

let generate_test oc =
  let fmt = Format.formatter_of_out_channel oc in
  let ms () = (Random.int max_matrix_size) + 1 in
  for i = 1 to test_size do
    Lacaml_Io.Toplevel.pp_fmat fmt (Lacaml.D.Mat.random (ms ()) (ms ()));
  done;
  for i = 1 to test_size do
    Lacaml_Io.Toplevel.pp_cmat fmt (Lacaml.Z.Mat.random (ms ()) (ms ()));
  done

let () =
  let oc, close =
    if Array.length Sys.argv > 1 then
      open_out Sys.argv.(1), close_out
    else
      stdout, (fun _ -> ())
  in
  Random.init (if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10);
  generate_test oc;
  close oc

