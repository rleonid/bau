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

  end

