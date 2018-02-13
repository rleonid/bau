(* Copyright {2016} {Leonid Rozenberg}

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

include
  (Bigarray : module type of Bigarray
  (* Thanks to Gabriel Scherer for explaining how to do this correctly:
    https://gitlab.com/gasche-snippets/overriding-bigarray-array1/blob/master/overriding_bigarray_array1.ml *)

  (* manual strengthening: preserve equalities of all phantom types *)
  with type float32_elt = Bigarray.float32_elt
  and type float64_elt = Bigarray.float64_elt
  and type int8_signed_elt = Bigarray.int8_signed_elt
  and type int8_unsigned_elt = Bigarray.int8_unsigned_elt
  and type int16_signed_elt = Bigarray.int16_signed_elt
  and type int16_unsigned_elt = Bigarray.int16_unsigned_elt
  and type int32_elt = Bigarray.int32_elt
  and type int64_elt = Bigarray.int64_elt
  and type int_elt = Bigarray.int_elt
  and type nativeint_elt = Bigarray.nativeint_elt
  and type complex32_elt = Bigarray.complex32_elt
  and type complex64_elt = Bigarray.complex64_elt
  and type c_layout = Bigarray.c_layout
  and type fortran_layout = Bigarray.fortran_layout

  (* manual strengthening: export equalities of GADTs *)
  and type 'c layout = 'c Bigarray.layout
  and type ('a, 'b) kind = ('a, 'b) Bigarray.kind

  (* the signature is now strengthened,
      and we can substitue modules with themselves *)

#if OCAML_VERSION >= (4, 05, 0)
  and module Array0 := Bigarray.Array0
#endif
  and module Array1 := Bigarray.Array1
  and module Array2 := Bigarray.Array2
  and module Array3 := Bigarray.Array3
  and module Genarray := Bigarray.Genarray)

module Layout = struct

  let is_fortran (type ll)(l : ll layout) =
    match l with
    | Fortran_layout -> true
    | C_layout       -> false

  let offset : type a. a layout -> int = function
    | Fortran_layout -> 1
    | C_layout       -> 0

  let foreach l n f =
    if is_fortran l then
      for i = 1 to n do f i done
    else
      for i = 0 to n - 1 do f i done

end (* Layout *)

module Kind = struct

  (* This module is copied from
   * https://github.com/hcarty/extbigarray/blob/master/src/extbigarray.ml
   * Modifications:
   * - aligned the arrows (stylistic nit)
   * - reordered Int to be the other cases.
   * - Added Char numerical methods (everything is done modulo 256).
   *)

  let to_char : type o r. (o, r) kind -> o -> char = function
    | Float32         -> fun x -> char_of_int (int_of_float x)
    | Float64         -> fun x -> char_of_int (int_of_float x)
    | Int8_signed     -> char_of_int
    | Int8_unsigned   -> char_of_int
    | Int16_signed    -> char_of_int
    | Int16_unsigned  -> char_of_int
    | Int             -> char_of_int
    | Int32           -> fun x -> char_of_int (Int32.to_int x)
    | Int64           -> fun x -> char_of_int (Int64.to_int x)
    | Nativeint       -> fun x -> char_of_int (Nativeint.to_int x)
    | Complex32       -> fun { Complex.re; _ } -> char_of_int (int_of_float re)
    | Complex64       -> fun { Complex.re; _ } -> char_of_int (int_of_float re)
    | Char            -> fun x -> x

  let of_char : type o r. (o, r) kind -> char -> o = function
    | Float32         -> fun x -> float_of_int (int_of_char x)
    | Float64         -> fun x -> float_of_int (int_of_char x)
    | Int8_signed     -> int_of_char
    | Int8_unsigned   -> int_of_char
    | Int16_signed    -> int_of_char
    | Int16_unsigned  -> int_of_char
    | Int             -> int_of_char
    | Int32           -> fun x -> Int32.of_int (int_of_char x)
    | Int64           -> fun x -> Int64.of_int (int_of_char x)
    | Nativeint       -> fun x -> Nativeint.of_int (int_of_char x)
    | Complex32       -> fun x ->
                          { Complex.re = float_of_int (int_of_char x); im = 0.0 }
    | Complex64       -> fun x ->
                          { Complex.re = float_of_int (int_of_char x); im = 0.0 }
    | Char            -> fun x -> x

  let to_add : type o r. (o, r) kind -> (o -> o -> o) = function
    | Float32         -> ( +. )
    | Float64         -> ( +. )
    | Int8_signed     -> ( + )
    | Int8_unsigned   -> ( + )
    | Int16_signed    -> ( + )
    | Int16_unsigned  -> ( + )
    | Int             -> ( + )
    | Int32           -> Int32.add
    | Int64           -> Int64.add
    | Nativeint       -> Nativeint.add
    | Complex32       -> Complex.add
    | Complex64       -> Complex.add
    | Char            -> fun c1 c2 -> Char.(chr ((code c1) + (code c2) mod 256))

  let to_sub : type o r. (o, r) kind -> (o -> o -> o) = function
    | Float32         -> ( -. )
    | Float64         -> ( -. )
    | Int8_signed     -> ( - )
    | Int8_unsigned   -> ( - )
    | Int16_signed    -> ( - )
    | Int16_unsigned  -> ( - )
    | Int             -> ( - )
    | Int32           -> Int32.sub
    | Int64           -> Int64.sub
    | Nativeint       -> Nativeint.sub
    | Complex32       -> Complex.sub
    | Complex64       -> Complex.sub
    | Char            -> fun c1 c2 -> Char.(chr ((code c1) - (code c2) mod 256))

  let to_mul : type o r. (o, r) kind -> (o -> o -> o) = function
    | Float32         -> ( *. )
    | Float64         -> ( *. )
    | Int8_signed     -> ( * )
    | Int8_unsigned   -> ( * )
    | Int16_signed    -> ( * )
    | Int16_unsigned  -> ( * )
    | Int             -> ( * )
    | Int32           -> Int32.mul
    | Int64           -> Int64.mul
    | Nativeint       -> Nativeint.mul
    | Complex32       -> Complex.mul
    | Complex64       -> Complex.mul
    | Char            -> fun c1 c2 -> Char.(chr ((code c1) * (code c2) mod 256))

  let to_div : type o r. (o, r) kind -> (o -> o -> o) = function
    | Float32         -> ( /. )
    | Float64         -> ( /. )
    | Int8_signed     -> ( / )
    | Int8_unsigned   -> ( / )
    | Int16_signed    -> ( / )
    | Int16_unsigned  -> ( / )
    | Int             -> ( / )
    | Int32           -> Int32.div
    | Int64           -> Int64.div
    | Nativeint       -> Nativeint.div
    | Complex32       -> Complex.div
    | Complex64       -> Complex.div
    | Char            -> fun c1 c2 -> Char.(chr ((code c1) / (code c2) mod 256))

  let to_neg : type o r. (o, r) kind -> (o -> o) = function
    | Float32         -> ( ~-. )
    | Float64         -> ( ~-. )
    | Int8_signed     -> ( ~- )
    | Int8_unsigned   -> ( ~- )
    | Int16_signed    -> ( ~- )
    | Int16_unsigned  -> ( ~- )
    | Int             -> ( ~- )
    | Int32           -> Int32.neg
    | Int64           -> Int64.neg
    | Nativeint       -> Nativeint.neg
    | Complex32       -> Complex.neg
    | Complex64       -> Complex.neg
    | Char            -> fun c1 -> Char.(chr (-(code c1) mod 256))

  let zero : type o r. (o, r) kind -> o = function
    | Float32         -> 0.0
    | Float64         -> 0.0
    | Int8_signed     -> 0
    | Int8_unsigned   -> 0
    | Int16_signed    -> 0
    | Int16_unsigned  -> 0
    | Int             -> 0
    | Int32           -> 0l
    | Int64           -> 0L
    | Nativeint       -> 0n
    | Complex32       -> Complex.zero
    | Complex64       -> Complex.zero
    | Char            -> '\000'

  let one : type o r. (o, r) kind -> o = function
    | Float32         -> 1.0
    | Float64         -> 1.0
    | Int8_signed     -> 1
    | Int8_unsigned   -> 1
    | Int16_signed    -> 1
    | Int16_unsigned  -> 1
    | Int             -> 1
    | Int32           -> 1l
    | Int64           -> 1L
    | Nativeint       -> 1n
    | Complex32       -> Complex.one
    | Complex64       -> Complex.one
    | Char            -> '\001'

  (* These functions were also in extbigarray, but commented out.
   * I've reviewed the implementation and corrected
   * - the unsigned min values.
   * - divided word_size by 8 bits -> bytes.
   *)
  let min_val : type o r. (o, r) kind -> o = function
    | Float32         -> neg_infinity
    | Float64         -> neg_infinity
    | Int8_signed     -> ~-128
    | Int8_unsigned   -> 0
    | Int16_signed    -> ~-32768
    | Int16_unsigned  -> 0
    | Int             -> min_int
    | Int32           -> Int32.min_int
    | Int64           -> Int64.min_int
    | Nativeint       -> Nativeint.min_int
    | Complex32       -> invalid_arg "min_val of Complex32"
    | Complex64       -> invalid_arg "min_val of Complex64"
    | Char            -> '\000'

  let max_val : type o r. (o, r) kind -> o = function
    | Float32         -> infinity
    | Float64         -> infinity
    | Int8_signed     -> 127
    | Int8_unsigned   -> 255
    | Int16_signed    -> 32767
    | Int16_unsigned  -> 65535
    | Int             -> max_int
    | Int32           -> Int32.max_int
    | Int64           -> Int64.max_int
    | Nativeint       -> Nativeint.max_int
    | Complex32       -> invalid_arg "max_val of Complex32"
    | Complex64       -> invalid_arg "max_val of Complex64"
    | Char            -> '\255'

  let bytes_per_element : type o r. (o, r) kind -> int = function
    | Float32         -> 4
    | Float64         -> 8
    | Int8_signed     -> 1
    | Int8_unsigned   -> 1
    | Int16_signed    -> 2
    | Int16_unsigned  -> 2
    | Int             -> Sys.word_size / 8    (* word_size in bits *)
    | Int32           -> 4
    | Int64           -> 8
    | Nativeint       -> Sys.word_size / 8
    | Complex32       -> 8
    | Complex64       -> 16
    | Char            -> 1

  let to_pp : type o r. (o, r) kind -> (Format.formatter -> o -> unit) = function
    | Float32         -> Format.pp_print_float
    | Float64         -> Format.pp_print_float
    | Int8_signed     -> Format.pp_print_int
    | Int8_unsigned   -> Format.pp_print_int
    | Int16_signed    -> Format.pp_print_int
    | Int16_unsigned  -> Format.pp_print_int
    | Int             -> Format.pp_print_int
    | Int32           -> fun fmt x -> Format.pp_print_string fmt (Int32.to_string x)
    | Int64           -> fun fmt x -> Format.pp_print_string fmt (Int64.to_string x)
    | Nativeint       -> fun fmt x -> Format.pp_print_string fmt (Nativeint.to_string x)
    | Complex32       -> let pp_sep fmt () = Format.pp_print_char fmt ',' in
                         fun fmt x ->
                           Format.pp_print_list ~pp_sep Format.pp_print_float fmt
                             Complex.[x.re; x.im]
    | Complex64       -> let pp_sep fmt () = Format.pp_print_char fmt ',' in
                         fun fmt x ->
                           Format.pp_print_list ~pp_sep Format.pp_print_float fmt
                             Complex.[x.re; x.im]
    | Char            -> Format.pp_print_char

end (* Kind *)

#if OCAML_VERSION >= (4, 05, 0)
module Array0 = struct
  include Bigarray.Array0

end (* Array0 *)
#endif

#define dimorphic(kind1,lkind1,kind2,lkind2)\
  module kind2 = struct \
    let fold_left2 ~f ~init u v   = [%open1.lkind1.lkind2 fold_left2 f init u v] \
    let fold_right2 ~f ~init u v  = [%open1.lkind1.lkind2 fold_right2 f init u v] \
    let foldi2 ~f ~init u v       = [%open1.lkind1.lkind2 foldi_left2 f init u v] \
    let iter2 ~f u v              = [%open1.lkind1.lkind2 iter2 f u v] \
    let iteri2 ~f u v             = [%open1.lkind1.lkind2 iteri2 f u v] \
    let map ~f u                  = [%open1.lkind1.lkind2 map f u] \
    let mapi ~f u                 = [%open1.lkind1.lkind2 mapi f u ] \
  end (* kind2 *)

#define monomorphic(kind,lowercase_kind)\
  module kind = struct \
    let fold_left ~f ~init v  = [%open1.lowercase_kind fold_left f init v] \
    let fold_right ~f ~init v = [%open1.lowercase_kind fold_right f init v] \
    let foldi ~f ~init v      = [%open1.lowercase_kind foldi_left f init v] \
    let reduce_left ~f v      = [%open1.lowercase_kind reduce_left f v] \
    let reduce_right ~f v     = [%open1.lowercase_kind reduce_right f v] \
    let reducei ~f v          = [%open1.lowercase_kind reducei_left f v] \
    let iter ~f v             = [%open1.lowercase_kind iter f v] \
    let iteri ~f v            = [%open1.lowercase_kind iteri f v] \
    let modify ~f v           = [%open1.lowercase_kind modify f v] \
    let modifyi ~f v          = [%open1.lowercase_kind modifyi f v] \
    let init ~f l n = \
      let v = create lowercase_kind l n in \
      modifyi ~f:(fun i _v -> f i) v; \
      v \
    let to_array v = \
      let o = Layout.offset (layout v) in \
      let a = Array.make (dim v) (unsafe_get v o) in \
      iteri v ~f:(fun i x -> Array.unsafe_set a (i - o) x); \
      a\
    let of_array a l = \
      let o = Layout.offset l in \
      let n = Array.length a in \
      init l n ~f:(fun i -> Array.unsafe_get a (i - o)) \
    module M2 = struct \
      dimorphic(kind,lowercase_kind,Float32,float32) \
      dimorphic(kind,lowercase_kind,Float64,float64) \
      dimorphic(kind,lowercase_kind,Int8_signed,int8_signed) \
      dimorphic(kind,lowercase_kind,Int8_unsigned,int8_unsigned) \
      dimorphic(kind,lowercase_kind,Int16_signed,int16_signed) \
      dimorphic(kind,lowercase_kind,Int16_unsigned,int16_unsigned) \
      dimorphic(kind,lowercase_kind,Int,int) \
      dimorphic(kind,lowercase_kind,Int32,int32) \
      dimorphic(kind,lowercase_kind,Int64,int64) \
      dimorphic(kind,lowercase_kind,Nativeint,nativeint) \
      dimorphic(kind,lowercase_kind,Complex32,complex32) \
      dimorphic(kind,lowercase_kind,Complex64,complex64) \
      dimorphic(kind,lowercase_kind,Char,char) \
    end (* M2 *) \
  end (* m *)

module Array1 = struct
  include Bigarray.Array1

  (* We need to have a generic version for now to have non-sequential
   * slicing, for Array2 (... and upwards).
   * *)
  let init k l n f =
    let m = create k l n in
    Layout.foreach l n (fun i -> set m i (f i));
    m

  let copy a =
    let b = create (kind a) (layout a) (dim a) in
    blit a b;
    b

  let to_array ~f a =
    let d = dim a in
    let o = Layout.offset (layout a) in
    Array.init d (fun i -> f (unsafe_get a (o + i)))

  monomorphic(Float32,float32)
  monomorphic(Float64,float64)
  monomorphic(Int8_signed,int8_signed)
  monomorphic(Int8_unsigned,int8_unsigned)
  monomorphic(Int16_signed,int16_signed)
  monomorphic(Int16_unsigned,int16_unsigned)
  monomorphic(Int,int)
  monomorphic(Int32,int32)
  monomorphic(Int64,int64)
  monomorphic(Nativeint,nativeint)
  monomorphic(Complex32,complex32)
  monomorphic(Complex64,complex64)
  monomorphic(Char,char)

end (* Array1 *)

module Array2 = struct
  include Bigarray.Array2

  let init k l d1 d2 f =
    let m = create k l d1 d2 in
    Layout.foreach l d1 (fun i ->
      Layout.foreach l d2 (fun j ->
        unsafe_set m i j (f i j)));
    m

  let to_array ~f a =
    let d1 = dim1 a in
    let d2 = dim2 a in
    let o = Layout.offset (layout a) in
    Array.init d1 (fun i ->
      Array.init d2 (fun j ->
        f (unsafe_get a (o + i) (o + j))))

  let natural_slice_indices (type l) (m : ('a, 'b, l) t) =
    match layout m with
    | Fortran_layout -> Array.init (dim2 m) (fun i -> i + 1)
    | C_layout       -> Array.init (dim1 m) (fun i -> i)

  (* Sequential access *)
  let slice (type l) (m : ('a, 'b, l) t) i : ('a, 'b, l) Array1.t =
    match layout m with
    | Fortran_layout -> slice_right m i
    | C_layout       -> slice_left m i

  (* Nonsequential slicing, requires a copy. *)
  let nonseq (type l) (m : ('a, 'b, l) t) i : ('a, 'b, l) Array1.t =
    let l = layout m in
    match l with
    | Fortran_layout -> Array1.init (kind m) l (dim2 m) (unsafe_get m i)
    | C_layout       -> Array1.init (kind m) l (dim1 m) (fun j -> unsafe_get m j i)

end (* Array2 *)

module Array3 = struct
  include Bigarray.Array3

  let init k l d1 d2 d3 f =
    let m = create k l d1 d2 d3 in
    Layout.foreach l d1 (fun i ->
      Layout.foreach l d2 (fun j ->
        Layout.foreach l d3 (fun k ->
          unsafe_set m i j k (f i j k))));
    m

  let to_array ~f a =
    let d1 = dim1 a in
    let d2 = dim2 a in
    let d3 = dim3 a in
    let o = Layout.offset (layout a) in
    Array.init d1 (fun i ->
      Array.init d2 (fun j ->
        Array.init d3 (fun k ->
          f (unsafe_get a (o + i) (o + j) (o + k)))))

  let natural_slice_indices (type l) (ar3 : ('a, 'b, l) t) =
    match layout ar3 with
    | Fortran_layout -> Array.init (dim3 ar3) (fun i -> i + 1)
    | C_layout       -> Array.init (dim1 ar3) (fun i -> i)

  let slice (type l) (ar3 : ('a, 'b, l) t) i : ('a, 'b, l) Array2.t =
    match layout ar3 with
    | Fortran_layout -> slice_right_2 ar3 i
    | C_layout       -> slice_left_2 ar3 i

  (* Nonsequential slicing, requires a copy. *)
  let nonseq (type l) (m : ('a, 'b, l) t) i : ('a, 'b, l) Array2.t =
    let l = layout m in
    match l with
    | Fortran_layout ->
      Array2.init (kind m) l (dim2 m) (dim3 m) (unsafe_get m i)
    | C_layout       ->
      Array2.init (kind m) l (dim1 m) (dim2 m) (fun j k -> unsafe_get m j k i)

end (* Array3 *)

module Genarray = struct
  include Bigarray.Genarray

  let foreachl dims l f =
    let num_dimensions = Array.length dims in
    let p = Array.fold_left ( * ) 1 dims in
    let z = Layout.offset l in
    let r = Array.make num_dimensions z in
    let x = Array.map (fun d -> d - 1 + z) dims in
    let succ () =
      let rec loop j =
        if j = num_dimensions then
          ()
        else if r.(j) = x.(j) then begin
          r.(j) <- z;
          loop (j + 1)
        end else
          r.(j) <- succ r.(j)
      in
      loop 0
    in
    for j = 0 to p - 1 do
      let () = f r in
      succ ()
    done

  let init k l d f =
    let m = create k l d in
    foreachl d l (fun arr -> set m arr (f arr));
    m

  (* Seems like the natural thing to do. *)
  let of_array ?dims k l arr =
    let ga = genarray_of_array1 (Array1.of_array k l arr) in
    match dims with
    | None -> ga
    | Some dims -> reshape ga dims

  let to_array ~f a =
    let d = dims a in
    let n = Array.fold_left ( * ) 1 d in
    Array1.to_array f (reshape_1 a n)

  let natural_slice_indices (type l) (ga : ('a, 'b, l) t) =
    let d = dims ga in
    let n = Array.length d in
    match layout ga with
    | Fortran_layout -> Array.init d.(n - 1) (fun i -> i + 1)
    | C_layout       -> Array.init d.(0) (fun i -> i)

  let slice (type l) (ga : ('a, 'b, l) t) i : ('a, 'b, l) t =
    match layout ga with
    | Fortran_layout -> slice_right ga [|i|]
    | C_layout       -> slice_left ga [|i|]

  (* Nonsequential slicing, requires a copy. *)
  let nonseq (type l) (ga : ('a, 'b, l) t) i : ('a, 'b, l) t =
    let d = dims ga in
    let n = Array.length d in
    let nm1 = n - 1 in
    let l = layout ga in
    match l with
    | Fortran_layout ->
        let nd = Array.sub d 1 nm1 in
        d.(0) <- i;
        let full idx = Array.blit idx 0 d 1 nm1; d in
        init (kind ga) l nd (fun idx -> get ga (full idx))
    | C_layout       ->
        let nd = Array.sub d 0 nm1 in
        d.(nm1) <- i;
        let full idx = Array.blit idx 0 d 0 nm1; d in
        init (kind ga) l nd (fun idx -> get ga (full idx))

end (* Genarray *)
