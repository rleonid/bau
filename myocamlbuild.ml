open Ocamlbuild_plugin

let () =
  let additional_rules =
    function
      | Before_hygiene  -> ()
      | After_hygiene   -> ()
      | Before_options  -> ()
      | After_options   -> ()
      | Before_rules    -> ()
      | After_rules     ->
          begin
            dep ["link";"ocaml";"use_fold"] ["src/lib/libfold.a"];
            flag ["link";"ocaml";"byte";"use_fold"]
              (S[A"-dllib";A"-lfold" ;A"-ccopt";A"-Lsrc/lib"     (* specify path to library *) ]);
          end
  in
  dispatch additional_rules
