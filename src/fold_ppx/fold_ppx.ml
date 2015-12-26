open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

let to_str ?(loc=Location.none) s = Location.mkloc s loc

let lid ?(loc=Location.none) s =
  Location.mkloc (Longident.parse s) loc

let ex_id ?(loc=Location.none) s =
  Exp.ident (lid ~loc s)

let make_fold_left var exp1 exp2 =
  Exp.let_ Nonrecursive
    [ Vb.mk (Pat.var (to_str "fold_left"))
        (Exp.fun_ "" None (Pat.var (to_str "a")) (exp1 ()))]
    (exp2 ())

let make_ref var init exp =
  Exp.let_ Nonrecursive
    [ Vb.mk (Pat.var (to_str var))
        (Exp.apply (ex_id "ref") ["", init])]
    (exp ())

let lookup_ref var =
  Exp.apply (ex_id "!") ["", (ex_id var)]

(* This is an operator! *)
let assign_ref var val_exp =
  Exp.apply (ex_id ":=") [("", (ex_id var)); ("", val_exp)]

let get_array1 arr index =
  Exp.apply (ex_id "Array1.unsafe_get") [("", (ex_id arr)); ("", (ex_id index))]

let apply_fold fold_f var arr index =
  Exp.apply fold_f [("", lookup_ref var); ("", get_array1 arr index)]

let make_for_loop index start_exp end_exp body_exp =
  Exp.for_ (Pat.var (to_str index)) start_exp end_exp Upto body_exp

let length_expr arr =
  Exp.apply (ex_id "Array1.dim") ["",(ex_id arr)]


let bigarray_fold_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
        Ast_helper.Exp.constant (Const_float "42.")
      | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr payload)} ->
        begin
        let n  = List.length payload in
        let () = Printf.eprintf "payload length: %d\n" n in
        match payload with
        | [{ pstr_desc = Pstr_eval
              ({pexp_desc = Pexp_apply
                    (fun_exp, [("", init); ("", v)]); _}, _); _ }] ->
            let open Ast_helper in
            let fold =
              make_fold_left "a" (fun () ->
                make_ref "r" init (fun () -> 
                  Exp.sequence 
                    (make_for_loop "i"
                      (Exp.constant (Const_int 1))
                      (length_expr "a")
                      (assign_ref "r" (apply_fold fun_exp "r" "a" "i"))
                    )
                    (lookup_ref "r")
                    ))
                (fun () -> 
                   Exp.apply (ex_id "fold_left") ["", v])
            in
            fold
            (*Exp.apply fun_exp [(li,init); (lv, v)] *)
        | _ ->
          Ast_helper.Exp.constant (Const_float "(-1.0)")
        end

      (*| { pexp_desc = Pexp_extension ({ txt; _ }, _) } ->
        match String.split "." 0 txt with
        | [ "array1"; "fold_left"; kind ] -> 
        | _ -> *)
      | other -> default_mapper.expr mapper other; }

let () =
  register "fold_ppx" bigarray_fold_mapper

(*
[%test (-) 3 4]
==>
{pexp_desc =
  Pexp_extension
   ({txt = "test"},
    PStr
     [{pstr_desc =
        Pstr_eval
         ({pexp_desc =
            Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "-"}},
             [("", {pexp_desc = Pexp_constant (Const_int 3)});
              ("", {pexp_desc = Pexp_constant (Const_int 4)})])},
         ...)}])}
=========

[%test (fun x -> x + 1) 3 4]
==>
{pexp_desc =
  Pexp_extension
   ({txt = "test"},
    PStr
     [{pstr_desc =
        Pstr_eval
         ({pexp_desc =
            Pexp_apply
             ({pexp_desc =
                Pexp_fun ("", None, {ppat_desc = Ppat_var {txt = "x"}},
                 {pexp_desc =
                   Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "+"}},
                    [("", {pexp_desc = Pexp_ident {txt = Lident "x"}});
                     ("", {pexp_desc = Pexp_constant (Const_int 1)})])})},
             [("", {pexp_desc = Pexp_constant (Const_int 3)});
              ("", {pexp_desc = Pexp_constant (Const_int 4)})])},
         ...)}])}

   *)
