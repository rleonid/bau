open Ocamlbuild_plugin

let () =
  dispatch
    (fun hook ->
      Ocamlbuild_cppo.dispatcher hook ;
    )
