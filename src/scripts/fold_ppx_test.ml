open Bigarrayo

let time s f =
  let n = Sys.time () in
  let r = f () in
  Printf.printf "%-30s:%f\n" s (Sys.time () -. n);
  r

let generate kind n =
  let gen  = Generators.random kind in
  let native = Array.init n (fun _ -> gen ()) in
  let f = Array1.of_array kind Fortran_layout native in
  let c = Array1.of_array kind C_layout native in
  native, f, c

let sum_n (v : float array) = Array.fold_left (+.) 0. v
let sum_b v = [%array1.fold_left.float64 (+.) 0. v]
let sum_b_f v = [%array1.fold_left.float64.fortran (+.) 0. v]
let sum_b_c v = [%array1.fold_left.float64.c (+.) 0. v]

(* TODO: Get to the bottom of if the differences are
   just due to comparing left vs right and different roundings.*)
let d = 1e-6
let significantly_different_from x y = y < (x -. d) || y > (x +. d )
let equal_floats x y = not (significantly_different_from x y)

let eq_array arr1 arr2 =
  Array.fold_left (fun (b, i) v ->
    b && equal_floats v arr2.(i), i + 1) (true, 0) arr1
  |> fst

let eq_array_c arr1 arr2 =
  Array.fold_left (fun (b, i) v ->
    b && equal_floats v.Complex.re arr2.(i).Complex.re
      && equal_floats v.Complex.im arr2.(i).Complex.im, i + 1) (true, 0) arr1
  |> fst


type ('a, 'b) pt =
  { sum_n : 'a array -> 'a
  ; kind  : ('a, 'b) kind
  ; sum_fl : ('a, 'b, fortran_layout) Array1.t -> 'a
  ; sum_fr : ('a, 'b, fortran_layout) Array1.t -> 'a
  ; sum_cl : ('a, 'b, c_layout) Array1.t -> 'a
  ; sum_cr : ('a, 'b, c_layout) Array1.t -> 'a
  ; eq     : 'a array -> 'a array -> bool
  }

let () =
  let samples, n =
    if Array.length Sys.argv < 3 then
      10000, 40
    else
      int_of_string Sys.argv.(1)
      , int_of_string Sys.argv.(2)
  in
  Printf.printf "%d samples of %d \n" samples n;
  let per : type a b. string -> (a, b) pt -> unit =
    fun ks pt ->
      Printf.printf "over ---- %s ----\n" ks;
      let data = Array.init samples (fun _ -> generate pt.kind n) in
      let test name op = time name (fun () -> Array.map op data) in
      let native  = test "native" (fun (n,_,_) -> pt.sum_n n) in
      let bgfolfl = test "array1 left fortran" (fun (_,f,_) -> pt.sum_fl f) in
      let bgfolfr = test "array1 right fortran" (fun (_,f,_) -> pt.sum_fr f) in
      let bgfolcl = test "array1 left c"       (fun (_,_,c) -> pt.sum_cl c) in
      let bgfolcr = test "array1 right c"       (fun (_,_,c) -> pt.sum_cr c) in
      Printf.printf "equal %b\n"
        ((pt.eq native bgfolfl)
        && (pt.eq bgfolfl bgfolfr)
        && (pt.eq bgfolfr bgfolcl)
        && (pt.eq bgfolcl bgfolcr))
  in
  per "float32"
    { sum_n = (fun (v : float array) -> Array.fold_left (+.) 0. v)
    ; kind  = Float32
    ; sum_fl = (fun v -> [%array1.fold_left.float32.fortran (+.) 0. v])
    ; sum_fr = (fun v -> [%array1.fold_right.float32.fortran (+.) 0. v])
    ; sum_cl = (fun v -> [%array1.fold_left.float32.c (+.) 0. v])
    ; sum_cr = (fun v -> [%array1.fold_right.float32.c (+.) 0. v])
    ; eq = eq_array
    };
  per "float64"
    { sum_n = (fun (v : float array) -> Array.fold_left (+.) 0. v)
    ; kind  = Float64
    ; sum_fl = (fun v -> [%array1.fold_left.float64.fortran (+.) 0. v])
    ; sum_fr = (fun v -> [%array1.fold_right.float64.fortran (+.) 0. v])
    ; sum_cl = (fun v -> [%array1.fold_left.float64.c (+.) 0. v])
    ; sum_cr = (fun v -> [%array1.fold_right.float64.c (+.) 0. v])
    ; eq = eq_array
    };
  per "complex32"
    { sum_n = (fun v  -> Array.fold_left Complex.add Complex.zero v)
    ; kind  = Complex32
    ; sum_fl = (fun v -> [%array1.fold_left.complex32.fortran Complex.add Complex.zero v])
    ; sum_fr = (fun v -> [%array1.fold_right.complex32.fortran Complex.add Complex.zero v])
    ; sum_cl = (fun v -> [%array1.fold_left.complex32.c Complex.add Complex.zero v])
    ; sum_cr = (fun v -> [%array1.fold_right.complex32.c Complex.add Complex.zero v])
    ; eq = eq_array_c
    };
  per "complex64"
    { sum_n = (fun v  -> Array.fold_left Complex.add Complex.zero v)
    ; kind  = Complex64
    ; sum_fl = (fun v -> [%array1.fold_left.complex64.fortran Complex.add Complex.zero v])
    ; sum_fr = (fun v -> [%array1.fold_right.complex64.fortran Complex.add Complex.zero v])
    ; sum_cl = (fun v -> [%array1.fold_left.complex64.c Complex.add Complex.zero v])
    ; sum_cr = (fun v -> [%array1.fold_right.complex64.c Complex.add Complex.zero v])
    ; eq = eq_array_c
    };
  per "int8_signed"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int8_signed
    ; sum_fl = (fun v -> [%array1.fold_left.int8_signed.fortran (+) 0 v])
    ; sum_fr = (fun v -> [%array1.fold_right.int8_signed.fortran (+) 0 v])
    ; sum_cl = (fun v -> [%array1.fold_left.int8_signed.c (+) 0 v])
    ; sum_cr = (fun v -> [%array1.fold_right.int8_signed.c (+) 0 v])
    ; eq = (=)
    };
  per "int8_unsigned"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int8_unsigned
    ; sum_fl = (fun v -> [%array1.fold_left.int8_unsigned.fortran (+) 0 v])
    ; sum_fr = (fun v -> [%array1.fold_right.int8_unsigned.fortran (+) 0 v])
    ; sum_cl = (fun v -> [%array1.fold_left.int8_unsigned.c (+) 0 v])
    ; sum_cr = (fun v -> [%array1.fold_right.int8_unsigned.c (+) 0 v])
    ; eq = (=)
    };
  per "int16_signed"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int16_signed
    ; sum_fl = (fun v -> [%array1.fold_left.int16_signed.fortran (+) 0 v])
    ; sum_fr = (fun v -> [%array1.fold_right.int16_signed.fortran (+) 0 v])
    ; sum_cl = (fun v -> [%array1.fold_left.int16_signed.c (+) 0 v])
    ; sum_cr = (fun v -> [%array1.fold_right.int16_signed.c (+) 0 v])
    ; eq = (=)
    };
  per "int16_unsigned"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int16_unsigned
    ; sum_fl = (fun v -> [%array1.fold_left.int16_unsigned.fortran (+) 0 v])
    ; sum_fr = (fun v -> [%array1.fold_right.int16_unsigned.fortran (+) 0 v])
    ; sum_cl = (fun v -> [%array1.fold_left.int16_unsigned.c (+) 0 v])
    ; sum_cr = (fun v -> [%array1.fold_right.int16_unsigned.c (+) 0 v])
    ; eq = (=)
    };
  per "int"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int
    ; sum_fl = (fun v -> [%array1.fold_left.int.fortran (+) 0 v])
    ; sum_fr = (fun v -> [%array1.fold_right.int.fortran (+) 0 v])
    ; sum_cl = (fun v -> [%array1.fold_left.int.c (+) 0 v])
    ; sum_cr = (fun v -> [%array1.fold_right.int.c (+) 0 v])
    ; eq = (=)
    };
  per "int32"
    { sum_n = (fun (v : int32 array) -> Array.fold_left Int32.add 0l v)
    ; kind  = Int32
    ; sum_fl = (fun v -> [%array1.fold_left.int32.fortran Int32.add 0l v])
    ; sum_fr = (fun v -> [%array1.fold_right.int32.fortran Int32.add 0l v])
    ; sum_cl = (fun v -> [%array1.fold_left.int32.c Int32.add 0l v])
    ; sum_cr = (fun v -> [%array1.fold_right.int32.c Int32.add 0l v])
    ; eq = (=)
    };
  per "int64"
    { sum_n = (fun (v : int64 array) -> Array.fold_left Int64.add 0L v)
    ; kind  = Int64
    ; sum_fl = (fun v -> [%array1.fold_left.int64.fortran Int64.add 0L v])
    ; sum_fr = (fun v -> [%array1.fold_right.int64.fortran Int64.add 0L v])
    ; sum_cl = (fun v -> [%array1.fold_left.int64.c Int64.add 0L v])
    ; sum_cr = (fun v -> [%array1.fold_right.int64.c Int64.add 0L v])
    ; eq = (=)
    };
  per "nativeint"
    { sum_n = (fun (v : Nativeint.t array) -> Array.fold_left Nativeint.add 0n v)
    ; kind  = Nativeint
    ; sum_fl = (fun v -> [%array1.fold_left.nativeint.fortran Nativeint.add 0n v])
    ; sum_fr = (fun v -> [%array1.fold_right.nativeint.fortran Nativeint.add 0n v])
    ; sum_cl = (fun v -> [%array1.fold_left.nativeint.c Nativeint.add 0n v])
    ; sum_cr = (fun v -> [%array1.fold_right.nativeint.c Nativeint.add 0n v])
    ; eq = (=)
    };
  let clor a b = (int_of_char a) lor (int_of_char b) |> Char.chr in
  per "char"
    { sum_n = (fun (v : char array) -> Array.fold_left clor '\000' v)
    ; kind  = Char
    ; sum_fl = (fun v -> [%array1.fold_left.char.fortran clor '\000' v])
    ; sum_fr = (fun v -> [%array1.fold_right.char.fortran clor '\000' v])
    ; sum_cl = (fun v -> [%array1.fold_left.char.c clor '\000' v])
    ; sum_cr = (fun v -> [%array1.fold_right.char.c clor '\000' v])
    ; eq = (=)
    }
