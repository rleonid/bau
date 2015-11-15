
module BigarrayO = struct
  include Bigarray 

  let isf (type ll)(l : ll layout) =
    match l with
    | Fortran_layout -> true
    | C_layout       -> false

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

    end

  module A2 = struct
    include Array2

    let init k l d1 d2 f =
      let m = create k l d1 d2 in
      foreach l d1 (fun i ->
        foreach l d2 (fun j ->
          set m i j (f i j)));
      m
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
    end

  module GA = struct
    include Genarray

    let foreachl dims f = 
      let rec loop acc =
        function
        | []      -> f (List.rev acc |> Array.of_list)
        | h :: tl ->
          for i = 1 to h do
            loop (i :: acc) tl
          done
      in
      loop [] (Array.to_list dims)


    let init k l d f =
      let m = create k l d in
      foreachl d (fun arr -> set m arr (f arr));
      m

    end


end

