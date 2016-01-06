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
type k = K : ('a, 'b) kind -> k

let parse_kind ~loc = function
  | "float32"         -> K Float32
  | "float64"         -> K Float64
  | "complex32"       -> K Complex32
  | "complex64"       -> K Complex64
  | "int8_signed"     -> K Int8_signed
  | "int8_unsigned"   -> K Int8_unsigned
  | "int16_signed"    -> K Int16_signed
  | "int16_unsigned"  -> K Int16_unsigned
  | "int32"           -> K Int32
  | "int64"           -> K Int64
  | "int"             -> K Int
  | "nativeint"       -> K Nativeint
  | "char"            -> K Char
  | ks                -> location_error ~loc "Unrecognized kind: %s" ks

let kind_to_types = function
  | K Float32         -> "float", "float32_elt"
  | K Float64         -> "float", "float64_elt"
  | K Int8_signed     -> "int", "int8_signed_elt"
  | K Int8_unsigned   -> "int", "int8_unsigned_elt"
  | K Int16_signed    -> "int", "int16_signed_elt"
  | K Int16_unsigned  -> "int", "int16_unsigned_elt"
  | K Int32           -> "int32", "int32_elt"
  | K Int64           -> "int64", "int64_elt"
  | K Int             -> "int", "int_elt"
  | K Nativeint       -> "nativeint", "nativeint_elt"
  | K Complex32       -> "Complex.t","complex32_elt"
  | K Complex64       -> "Complex.t","complex64_elt"
  | K Char            -> "char", "int8_unsigned_elt"

type l = L : 'a layout -> l

let parse_layout_str ~loc = function
  | "fortran" -> L Fortran_layout
  | "c"       -> L C_layout
  | ls        -> location_error ~loc "Unrecognized layout: %s" ls

let to_fold_params = function
  | L Fortran_layout -> "fortran_layout", 1, false
  | L C_layout       -> "c_layout", 0, true

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

let operation_to_name_n_body fun_exp init = function
  | Iter       -> "iter",       (iter_body ~upto:true fun_exp)
  | Fold true  -> "fold_left",  (fold_body ~upto:true fun_exp init)
  | Fold false -> "fold_right", (fold_body ~upto:false fun_exp init)

(* Create a fast iter/fold using a reference and for-loop. *)
let create_layout_specific (f, init, v) op kind layout =
  let open Ast_helper in
  let layout, start, minus_one = to_fold_params layout in
  let name, body = operation_to_name_n_body f init op in
  make_let ~layout kind name (body ~start ~minus_one) (Exp.apply (ex_id name) ["", v])

(* Create a layout agnostic fold/iter function. *)
let create (fun_exp, init, v) op kind =
  let name, body = operation_to_name_n_body fun_exp init op in
  let name_f = name ^ "_fortran" in
  let name_c = name ^ "_c" in
  make_let ~arg:"b" kind name
    (let layout, start, minus_one = to_fold_params (L Fortran_layout) in
    make_let ~layout kind name_f (body ~start ~minus_one)
      (* intended variable masking *)
      (let layout, start, minus_one = to_fold_params (L C_layout) in
      make_let ~layout kind name_c (body ~start ~minus_one)
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

let parse_operation ~loc = function
  | "iter"       -> Iter
  | "fold_left"  -> Fold true
  | "fold_right" -> Fold false
  | operation    -> location_error ~loc "Unrecognized command: %s" operation

let parse ?layout loc operation ks payload =
  try
    let op   = parse_operation ~loc operation in
    let kind = parse_kind ~loc ks in
    match layout with
    | None ->
      let t = parse_payload loc "array1" operation payload in
      create t op kind
    | Some ls ->
      let layout = parse_layout_str ~loc ls in
      let t = parse_payload loc "array1" operation payload in
      create_layout_specific t op kind layout
  with Location.Error e ->
    Exp.extension ~loc (extension_of_error e)

let transform loc txt payload def =
  match split '.' txt with
  | ["array1"; oper; kind]         -> parse loc oper kind payload
  | ["array1"; oper; kind; layout] -> parse ~layout loc oper kind payload
  | _ -> def ()

let bigarray_fold_mapper _argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt }, PStr payload)} ->
          transform expr.pexp_loc txt payload (fun () -> default_mapper.expr mapper expr)
      | other -> default_mapper.expr mapper other; }

let () =
  run_main bigarray_fold_mapper
