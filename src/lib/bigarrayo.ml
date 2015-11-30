include Bigarray

let isf (type ll)(l : ll layout) =
  match l with
  | Fortran_layout -> true
  | C_layout       -> false

let to_offset : type a. a layout -> (int -> int) = function
  | Fortran_layout -> (fun i -> i + 1)
  | C_layout       -> (fun i -> i)

let foreach l n f =
  if isf l then
    for i = 1 to n do f i done
  else
    for i = 0 to n - 1 do f i done

(* Destructive substitution of modules isn't working out...
    Probably hard to pull apart the dependency? for all the methods
    that use Array1 explicitly.*)

module A1 = struct
  include Array1

  let init k l n f =
    let m = create k l n in
    foreach l n (fun i -> set m i (f i));
    m

  let to_array ~f a =
    let d = dim a in
    let o = to_offset (layout a) in
    Array.init d (fun i -> f (unsafe_get a (o i)))

  end

module A2 = struct
  include Array2

  let init k l d1 d2 f =
    let m = create k l d1 d2 in
    foreach l d1 (fun i ->
      foreach l d2 (fun j ->
        set m i j (f i j)));
    m

  let to_array ~f a =
    let d1 = dim1 a in
    let d2 = dim2 a in
    let o = to_offset (layout a) in
    Array.init d1 (fun i ->
      Array.init d2 (fun j ->
        f (unsafe_get a (o i) (o j))))

  let natural_slice_indices (type l) (m : ('a, 'b, l) t) =
    match layout m with
    | Fortran_layout -> Array.init (dim2 m) (fun i -> i + 1)
    | C_layout       -> Array.init (dim1 m) (fun i -> i)

  (* Sequential access *)
  let slice (type l) (m : ('a, 'b, l) t) i : ('a, 'b, l) A1.t =
    match layout m with
    | Fortran_layout -> slice_right m i
    | C_layout       -> slice_left m i

  (* Nonsequential slicing, requires a copy. *)
  let nonseq (type l) (m : ('a, 'b, l) t) i : ('a, 'b, l) A1.t =
    let l = layout m in
    match l with
    | Fortran_layout -> A1.init (kind m) l (dim2 m) (unsafe_get m i)
    | C_layout       -> A1.init (kind m) l (dim1 m) (fun j -> unsafe_get m j i)

  end

module A3 = struct
  include Array3

  let init k l d1 d2 d3 f =
    let m = create k l d1 d2 d3 in
    foreach l d1 (fun i ->
      foreach l d2 (fun j ->
        foreach l d3 (fun k ->
          set m i j k (f i j k))));
    m

  let to_array ~f a =
    let d1 = dim1 a in
    let d2 = dim2 a in
    let d3 = dim3 a in
    let o = to_offset (layout a) in
    Array.init d1 (fun i ->
      Array.init d2 (fun j ->
        Array.init d3 (fun k ->
        f (unsafe_get a (o i) (o j) (o k)))))

  let natural_slice_indices (type l) (ar3 : ('a, 'b, l) t) =
    match layout ar3 with
    | Fortran_layout -> Array.init (dim3 ar3) (fun i -> i + 1)
    | C_layout       -> Array.init (dim1 ar3) (fun i -> i)

  let slice (type l) (ar3 : ('a, 'b, l) t) i : ('a, 'b, l) A2.t =
    match layout ar3 with
    | Fortran_layout -> slice_right_2 ar3 i
    | C_layout       -> slice_left_2 ar3 i

  (* Nonsequential slicing, requires a copy. *)
  let nonseq (type l) (m : ('a, 'b, l) t) i : ('a, 'b, l) A2.t =
    let l = layout m in
    match l with
    | Fortran_layout ->
      A2.init (kind m) l (dim2 m) (dim3 m) (unsafe_get m i)
    | C_layout       ->
      A2.init (kind m) l (dim1 m) (dim2 m) (fun j k -> unsafe_get m j k i)

  end

module GA = struct
  include Genarray

  let foreachl dims isf f =
    let rec loop acc =
      function
      | []      -> f (List.rev acc |> Array.of_list)
      | h :: tl ->
        if isf then
          for i = 1 to h do
            loop (i :: acc) tl
          done
        else
          for i = 0 to h - 1 do
            loop (i :: acc) tl
          done
    in
    loop [] (Array.to_list dims)

  let init k l d f =
    let m = create k l d in
    foreachl d (isf l) (fun arr -> set m arr (f arr));
    m

  let to_array ~f a =
    let d = dims a in
    let n = Array.fold_left ( * ) 1 d in
    A1.to_array f (reshape_1 a n)

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
  end

(** Fold *)
external num_elements : ('a, 'b, 'c) Genarray.t -> int = "num_elements"

