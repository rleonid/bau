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

let location_error ?(sub=[]) ?(if_highlight="") ~loc fmt =
  Printf.ksprintf (fun msg ->
    raise Location.(Error { loc; msg; sub; if_highlight; }))
    fmt

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

let make_let ?layout ?(arg="a") kind fold_var exp1 app =
  let to_body ls = Exp.fun_ "" None (constrain_vec kind ls arg) exp1 in
  let body =
    match layout with
    | None    -> Exp.newtype "l" (to_body "l")
    | Some ls -> to_body ls
  in
  Exp.let_ Nonrecursive [ Vb.mk (Pat.var (to_str fold_var)) body] app

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

let apply_f fold_f args =
  Exp.apply fold_f (List.map (fun e -> "",e) args)

(*let apply_f ~upto fold_f var arr index =
  if upto
  then Exp.apply fold_f [("", lookup_ref var); ("", get_array1 arr index)]
  else Exp.apply fold_f [("", get_array1 arr index); ("", lookup_ref var)] *)

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

type operation =
  | Iter
  | Fold of bool    (* upto ie fold_left *)

let fold_apply_f ~upto fun_exp ~ref ~arr ~index =
  if upto then
    apply_f fun_exp [ lookup_ref ref; get_array1 arr index]
  else
    apply_f fun_exp [ get_array1 arr index; lookup_ref ref]

let fold_body ?(vec_arg="a") ~upto ~start ~minus_one fun_exp init =
  (make_ref "r" init
     (Exp.sequence
        (make_for_loop "i"
          (Exp.constant (Const_int start))
          (length_expr ~minus_one vec_arg)
          upto
          (assign_ref "r"
            (fold_apply_f ~upto fun_exp ~ref:"r" ~arr:vec_arg ~index:"i")))
        (lookup_ref "r")))

let iter_body ?(vec_arg="a") ~upto ~start ~minus_one fun_exp =
  make_for_loop "i"
    (Exp.constant (Const_int start))
    (length_expr ~minus_one vec_arg)
    upto
    (apply_f fun_exp [get_array1 vec_arg "i"])

(* Create a fast iter/fold using a reference and for-loop. *)
let create_layout_specific (f, init, v) op kind layout =
  let open Ast_helper in
  let layout, start, minus_one = layout_to_fold_parameters layout in
    let name, body  =
    match op with
    | Iter       -> "iter",       iter_body ~upto:true  ~start ~minus_one f
    | Fold true  -> "fold_left",  fold_body ~upto:true  ~start ~minus_one f init
    | Fold false -> "fold_right", fold_body ~upto:false ~start ~minus_one f init
  in
  make_let ~layout kind name body (Exp.apply (ex_id name) ["", v])

(* Create a layout agnostic fold/iter function. *)
let create (fun_exp, init, v) op kind =
  let name, name_f, name_c, to_body =
    match op with
    | Iter ->
      "iter", "iter_fortran", "iter_c"
      , (iter_body ~upto:true fun_exp)
    | Fold true  ->
      "fold_left", "fold_left_fortran", "fold_left_c"
      , (fold_body ~upto:true fun_exp init)
    | Fold false -> "fold_right", "fold_right_fortran", "fold_right_c"
      , (fold_body ~upto:false fun_exp init)
  in
  make_let ~arg:"b" kind name
    (let layout, start, minus_one = layout_to_fold_parameters Fortran_layout in
    make_let ~layout kind name_f (to_body ~start ~minus_one)
      (* intended variable masking *)
      (let layout, start, minus_one = layout_to_fold_parameters C_layout in
      make_let ~layout kind name_c (to_body ~start ~minus_one)
        (Exp.match_ (Exp.apply (ex_id "Array1.layout") ["", (ex_id "b")])
          [ Exp.case (Pat.construct (lid "Fortran_layout") None)
              (Exp.apply (ex_id name_f) ["", (ex_id "b")])
          ; Exp.case (Pat.construct (lid "C_layout") None)
              (Exp.apply (ex_id name_c) ["", (ex_id "b")])])))
    (Exp.apply (ex_id name) ["", v])

let parse_payload loc ba_type f = function
  | [{ pstr_desc = Pstr_eval
        ({pexp_desc = Pexp_apply
              (fun_exp, [("", init); ("", v)]); _}, _); _ }] ->
    (fun_exp, init, v)
  | [{ pstr_desc = Pstr_eval
        ({pexp_desc = Pexp_apply
              (fun_exp, [("", init); ]); _}, _); _ }] ->
      location_error ~loc "Missing %s argument to %s" ba_type f
  | [{ pstr_desc = Pstr_eval
        ({pexp_desc = _}, _); _}] ->
      location_error ~loc "Missing init and %s argument to %s" ba_type f
  | _ -> location_error ~loc "Missing %s arguments" f

let parse_operation = function
  | "iter"       -> Some Iter
  | "fold_left"  -> Some (Fold true)
  | "fold_right" -> Some (Fold false)
  | _            -> None

let parse loc operation ks ls_opt payload =
  match parse_operation operation with
  | None -> location_error ~loc "Unrecognized command: %s" operation
  | Some op ->
    match parse_kind ks with
    | None -> location_error ~loc "Unrecognized kind: %s" ks
    | Some kind ->
      match ls_opt with
      | None ->
        let t = parse_payload loc "array1" operation payload in
        create t op kind
      | Some ls ->
        match parse_layout_str ls with
        | None -> location_error ~loc "Unrecognized layout: %s" ls
        | Some layout ->
          let t = parse_payload loc "array1" operation payload in
          create_layout_specific t op kind layout

let transform loc txt payload def =
  match split '.' txt with
  | ["array1"; oper; kind]     -> parse loc oper kind None payload
  | ["array1"; oper; kind; ls] -> parse loc oper kind (Some ls) payload
  | _ -> def ()

let bigarray_fold_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt }, PStr payload)} ->
          transform expr.pexp_loc txt payload (fun () -> default_mapper.expr mapper expr)
      | other -> default_mapper.expr mapper other; }

let () =
  register "fold_ppx" bigarray_fold_mapper
