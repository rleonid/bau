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

let parse_kind loc = function
    | "float32"         -> Float32
    | "float64"         -> Float64
    | "complex32"       -> Complex32
    | "complex64"       -> Complex64
    | "int8_signed"     -> Int8_signed
    | "int8_unsigned"   -> Int8_unsigned
    | "int16_signed"    -> Int16_signed
    | "int16_unsigned"  -> Int16_unsigned
    | "int32"           -> Int32
    | "int64"           -> Int64
    | "int"             -> Int
    | "nativeint"       -> Nativeint
    | "char"            -> Char
    | x                 ->
      location_error loc (sprintf "unrecognized %s Bigarray kind" x)

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

type bigarray_layout =
  | Fortran_layout
  | C_layout

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

let make_fold_left kind layout_opt fold_var vec_var exp1 exp2 =
  let to_body ls =  Exp.fun_ "" None (constrain_vec kind ls vec_var) exp1 in
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

let apply_f fold_f var arr index =
  Exp.apply fold_f [("", lookup_ref var); ("", get_array1 arr index)]

let make_for_loop index start_exp end_exp body_exp =
  Exp.for_ (Pat.var (to_str index)) start_exp end_exp Upto body_exp

let length_expr ~minus_one arr =
  if minus_one then
    Exp.apply (ex_id "-")
      [ "", (Exp.apply (ex_id "Array1.dim") ["",(ex_id arr)])
      ; "", (Exp.constant (Const_int 1))]
  else
    Exp.apply (ex_id "Array1.dim") ["",(ex_id arr)]

(* Create a fast fold using a reference and for-loop. *)
let create_fold kind layout (fun_exp, init, v) =
  let open Ast_helper in
  match layout with
  | Some l ->
    let layouts, s, minus_one = layout_to_fold_parameters l in
    make_fold_left kind (Some layouts) "fold_left" "a"
      (make_ref "r" init
        (Exp.sequence
          (make_for_loop "i"
            (Exp.constant (Const_int s))
            (length_expr ~minus_one "a")
            (assign_ref "r" (apply_f fun_exp "r" "a" "i")))
          (lookup_ref "r")))
      (Exp.apply (ex_id "fold_left") ["", v])
  | None  ->  (* Create a layout agnostic function. *)
    make_fold_left kind None "fold_left" "b"
      (let layouts, s, minus_one = layout_to_fold_parameters Fortran_layout in
      make_fold_left kind (Some layouts) "fold_left_fortran" "a"
        (make_ref "r" init (
          Exp.sequence
            (make_for_loop "i"
              (Exp.constant (Const_int s))
              (length_expr ~minus_one "a")
              (assign_ref "r" (apply_f fun_exp "r" "a" "i")))
            (lookup_ref "r")))
        (let layouts, s, minus_one = layout_to_fold_parameters C_layout in
          make_fold_left kind (Some layouts) "fold_left_c" "a" (
          make_ref "r" init (
            Exp.sequence
              (make_for_loop "i"
                (Exp.constant (Const_int s))
                (length_expr ~minus_one "a")
                (assign_ref "r" (apply_f fun_exp "r" "a" "i")))
              (lookup_ref "r")))
            (Exp.match_ (Exp.apply (ex_id "Array1.layout") ["", (ex_id "b")])
              [ Exp.case (Pat.construct (lid "Fortran_layout") None)
                  (Exp.apply (ex_id "fold_left_fortran") ["", (ex_id "b")])
              ; Exp.case (Pat.construct (lid "C_layout") None)
                  (Exp.apply (ex_id "fold_left_c") ["", (ex_id "b")])])))
      (Exp.apply (ex_id "fold_left") ["", v])

let parse_payload = function
  | [{ pstr_desc = Pstr_eval
        ({pexp_desc = Pexp_apply
              (fun_exp, [("", init); ("", v)]); _}, _); _ }] ->
    Some (fun_exp, init, v)
  | _ -> None

let transform loc txt payload fail =
  match parse_payload payload with
  | None   -> fail ()
  | Some t ->
    match split '.' txt with
    | ["array1"; "fold_left"; kind_str] ->
      let kind = parse_kind loc kind_str in
      create_fold kind None t
    | ["array1"; "fold_left"; kind_str; ls] ->
      begin
      match parse_layout_str ls with
      | Some l ->
        let kind = parse_kind loc kind_str in
        create_fold kind None t
      | None -> fail () (* TODO: change to explicit failure *)
      end
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
