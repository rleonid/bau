

open Bau.BigarrayO

let test_size = 10
let max_matrix_size = 10

let ms () = (Random.int max_matrix_size) + 1
let rf () = Random.float 1.0

let double_mat n m =
  A2.init Float64 Fortran_layout n m (fun _ _ -> rf ())

let complex_mat n m =
  A2.init Complex64 Fortran_layout n m
    (fun _ _ -> {Complex.re = rf (); im = rf ()})

let generate_test oc =
  let fmt = Format.formatter_of_out_channel oc in
  for i = 1 to test_size do
    Lacaml_io.Toplevel.pp_fmat fmt (double_mat (ms ()) (ms ()));
  done;
  for i = 1 to test_size do
    Lacaml_io.Toplevel.pp_cmat fmt (complex_mat (ms ()) (ms ()));
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

