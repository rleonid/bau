open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

open Printf
open Bigarray

(* Utilities *)
let split c s =
  let rec loop o =
    try
      let i = String.index_from s o c in
      (String.sub s o (i - o)) :: (loop (i + 1))
    with Not_found ->
      [String.sub s o (String.length s - o)]
  in
  loop 0

let location_error ?(sub=[]) ?(if_highlight="") loc msg =
  raise Location.(Error { loc; msg; sub; if_highlight; })

(* Bigarray specific transforms *)
(* Is there a better way to handle the GADT's in Bigarray that
   represent this as opposed to redefining them? *)
type kinds =
  | Float32
  | Float64
  | Complex32
  | Complex64
  | Int8_signed
  | Int8_unsigned
  | Int16_signed
  | Int16_unsigned
  | Int32
  | Int64
  | Int
  | Nativeint
  | Char

type bigarray_layout =
  | Fortran_layout
  | C_layout

let parse_kind = function
  | "float32"         -> Some Float32
  | "float64"         -> Some Float64
  | "complex32"       -> Some Complex32
  | "complex64"       -> Some Complex64
  | "int8_signed"     -> Some Int8_signed
  | "int8_unsigned"   -> Some Int8_unsigned
  | "int16_signed"    -> Some Int16_signed
  | "int16_unsigned"  -> Some Int16_unsigned
  | "int32"           -> Some Int32
  | "int64"           -> Some Int64
  | "int"             -> Some Int
  | "nativeint"       -> Some Nativeint
  | "char"            -> Some Char
  | _                 -> None

let kind_to_types = function
  | Float32         -> "float", "float32_elt"
  | Float64         -> "float", "float64_elt"
  | Int8_signed     -> "int", "int8_signed_elt"
  | Int8_unsigned   -> "int", "int8_unsigned_elt"
  | Int16_signed    -> "int", "int16_signed_elt"
  | Int16_unsigned  -> "int", "int16_unsigned_elt"
  | Int32           -> "int32", "int32_elt"
  | Int64           -> "int64", "int64_elt"
  | Int             -> "int", "int_elt"
  | Nativeint       -> "nativeint", "nativeint_elt"
  | Complex32       -> "Complex.t","complex32_elt"
  | Complex64       -> "Complex.t","complex64_elt"
  | Char            -> "char", "int8_unsigned_elt"

let parse_layout_str = function
  | "fortran" -> Some Fortran_layout
  | "c"       -> Some C_layout
  | _         -> None

let layout_to_fold_parameters = function
  | Fortran_layout -> "fortran_layout", 1, false
  | C_layout       -> "c_layout", 0, true

(* AST construction helpers *)
let to_str ?(loc=Location.none) s = Location.mkloc s loc

let lid ?(loc=Location.none) s =
  Location.mkloc (Longident.parse s) loc

let ex_id ?(loc=Location.none) s =
  Exp.ident (lid ~loc s)

let constrain_vec kind layout_s vec_var =
  let t1, t2 = kind_to_types kind in
  let econstr s = Typ.constr (lid s) [] in
  Pat.constraint_ (Pat.var (to_str vec_var))
    (Typ.constr (lid "Array1.t")
       [ econstr t1; econstr t2; econstr layout_s])

let make_fold kind layout_opt fold_var vec_var exp1 exp2 =
  let to_body ls = Exp.fun_ "" None (constrain_vec kind ls vec_var) exp1 in
  let body =
    match layout_opt with
    | None    -> Exp.newtype "l" (to_body "l")
    | Some ls -> to_body ls
  in
  Exp.let_ Nonrecursive [ Vb.mk (Pat.var (to_str fold_var)) body] exp2

let make_ref var init exp =
  Exp.let_ Nonrecursive
    [ Vb.mk (Pat.var (to_str var))
        (Exp.apply (ex_id "ref") ["", init])]
    exp

let lookup_ref var =
  Exp.apply (ex_id "!") ["", (ex_id var)]

(* This is an operator! *)
let assign_ref var val_exp =
  Exp.apply (ex_id ":=") [("", (ex_id var)); ("", val_exp)]

let get_array1 arr index =
  Exp.apply (ex_id "Array1.unsafe_get") [("", (ex_id arr)); ("", (ex_id index))]

let apply_f ~upto fold_f var arr index =
  if upto
  then Exp.apply fold_f [("", lookup_ref var); ("", get_array1 arr index)]
  else Exp.apply fold_f [("", get_array1 arr index); ("", lookup_ref var)]

let make_for_loop index start_exp end_exp upto body_exp =
  if upto
  then Exp.for_ (Pat.var (to_str index)) start_exp end_exp Upto body_exp
  else Exp.for_ (Pat.var (to_str index)) end_exp start_exp Downto body_exp

let length_expr ~minus_one arr =
  if minus_one then
    Exp.apply (ex_id "-")
      [ "", (Exp.apply (ex_id "Array1.dim") ["",(ex_id arr)])
      ; "", (Exp.constant (Const_int 1))]
  else
    Exp.apply (ex_id "Array1.dim") ["",(ex_id arr)]

(* Create a fast fold using a reference and for-loop. *)
let create_fold_w_layout (fun_exp, init, v) upto kind layout =
  let open Ast_helper in
  let layouts, s, minus_one = layout_to_fold_parameters layout in
  let name = if upto then "fold_left" else "fold_right" in
  make_fold kind (Some layouts) name "a"
    (make_ref "r" init
      (Exp.sequence
        (make_for_loop "i"
          (Exp.constant (Const_int s))
          (length_expr ~minus_one "a")
          upto
          (assign_ref "r" (apply_f ~upto fun_exp "r" "a" "i")))
        (lookup_ref "r")))
    (Exp.apply (ex_id name) ["", v])

(* Create a layout agnostic function. *)
let create_fold (fun_exp, init, v) upto kind =
  let name, name_f, name_c =
    if upto
    then "fold_left", "fold_left_fortran", "fold_left_c"
    else "fold_right", "fold_right_fortran", "fold_right_c"
  in
  make_fold kind None name "b"
    (let layouts, s, minus_one = layout_to_fold_parameters Fortran_layout in
    make_fold kind (Some layouts) name_f "a"
      (make_ref "r" init
        (Exp.sequence
          (make_for_loop "i"
            (Exp.constant (Const_int s))
            (length_expr ~minus_one "a")
            upto
            (assign_ref "r" (apply_f ~upto fun_exp "r" "a" "i")))
          (lookup_ref "r")))
      (let layouts, s, minus_one = layout_to_fold_parameters C_layout in
      make_fold kind (Some layouts) name_c "a"
        (make_ref "r" init
          (Exp.sequence
            (make_for_loop "i"
              (Exp.constant (Const_int s))
              (length_expr ~minus_one "a")
              upto
              (assign_ref "r" (apply_f ~upto fun_exp "r" "a" "i")))
            (lookup_ref "r")))
          (Exp.match_ (Exp.apply (ex_id "Array1.layout") ["", (ex_id "b")])
            [ Exp.case (Pat.construct (lid "Fortran_layout") None)
                (Exp.apply (ex_id name_f) ["", (ex_id "b")])
            ; Exp.case (Pat.construct (lid "C_layout") None)
                (Exp.apply (ex_id name_c) ["", (ex_id "b")])])))
    (Exp.apply (ex_id name) ["", v])

let parse_payload = function
  | [{ pstr_desc = Pstr_eval
        ({pexp_desc = Pexp_apply
              (fun_exp, [("", init); ("", v)]); _}, _); _ }] ->
    Some (fun_exp, init, v)
  | _ -> None

let parse_fold = function
  | "fold_left"  -> Some true
  | "fold_right" -> Some false
  | _            -> None

let parse_command t fs ks ls_opt fail =
  match parse_fold fs with
  | None -> fail ()
  | Some direction ->
    match parse_kind ks with
    | None -> fail ()
    | Some kind ->
      match ls_opt with
      | None -> create_fold t direction kind
      | Some ls ->
        match parse_layout_str ls with
        | None -> fail ()
        | Some layout -> create_fold_w_layout t direction kind layout

let transform loc txt payload fail =
  match parse_payload payload with
  | None   -> fail ()
  | Some t ->
    match split '.' txt with
    | ["array1"; fold; kind] ->
      parse_command t fold kind None fail
    | ["array1"; fold; kind; ls] ->
      parse_command t fold kind (Some ls) fail
    | _ -> fail ()

let bigarray_fold_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt }, PStr payload)} ->
          transform expr.pexp_loc txt payload (fun () -> default_mapper.expr mapper expr)
      | other -> default_mapper.expr mapper other; }

let () =
  register "fold_ppx" bigarray_fold_mapper
