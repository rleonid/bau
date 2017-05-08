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

  let is_fortran_layout (type ll)(l : ll layout) =
    match l with
    | Fortran_layout -> true
    | C_layout       -> false

  let to_offset : type a. a layout -> int = function
    | Fortran_layout -> 1
    | C_layout       -> 0

  let foreach l n f =
    if is_fortran_layout l then
      for i = 1 to n do f i done
    else
      for i = 0 to n - 1 do f i done
end

#if OCAML_VERSION >= (4, 05, 0)
module Array0 = struct
  include Bigarray.Array0

end (* Array0 *)
#endif

module Array1 = struct
  include Bigarray.Array1

  let init k l n f =
    let m = create k l n in
    Layout.foreach l n (fun i -> unsafe_set m i (f i));
    m

  let to_array ~f a =
    let d = dim a in
    let o = Layout.to_offset (layout a) in
    Array.init d (fun i -> f (unsafe_get a (o + i)))

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
    let o = Layout.to_offset (layout a) in
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
    let o = Layout.to_offset (layout a) in
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
    let z = Layout.to_offset l in
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
