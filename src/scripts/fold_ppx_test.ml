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

type ('a, 'b) pt =
  { sum_n : 'a array -> 'a
  ; kind  : ('a, 'b) kind
  ; sum_f : ('a, 'b, fortran_layout) Array1.t -> 'a
  ; sum_c : ('a, 'b, c_layout) Array1.t -> 'a
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
      let bgfolf  = test "array1 fold fortran" (fun (_,f,_) -> pt.sum_f f) in
      let bgfolc  = test "array1 fold c"       (fun (_,_,c) -> pt.sum_c c) in
      Printf.printf "equal %b\n" (native = bgfolf && bgfolf = bgfolc)
  in
  per "float32"
    { sum_n = (fun (v : float array) -> Array.fold_left (+.) 0. v)
    ; kind  = Float32
    ; sum_f = (fun v -> [%array1.fold_left.float32.fortran (+.) 0. v])
    ; sum_c = (fun v -> [%array1.fold_left.float32.c (+.) 0. v])
    };
  per "float64"
    { sum_n = (fun (v : float array) -> Array.fold_left (+.) 0. v)
    ; kind  = Float64
    ; sum_f = (fun v -> [%array1.fold_left.float64.fortran (+.) 0. v])
    ; sum_c = (fun v -> [%array1.fold_left.float64.c (+.) 0. v])
    };
  per "complex32"
    { sum_n = (fun v  -> Array.fold_left Complex.add Complex.zero v)
    ; kind  = Complex32
    ; sum_f = (fun v -> [%array1.fold_left.complex32.fortran Complex.add Complex.zero v])
    ; sum_c = (fun v -> [%array1.fold_left.complex32.c Complex.add Complex.zero v])
    };
  per "complex64"
    { sum_n = (fun v  -> Array.fold_left Complex.add Complex.zero v)
    ; kind  = Complex64
    ; sum_f = (fun v -> [%array1.fold_left.complex64.fortran Complex.add Complex.zero v])
    ; sum_c = (fun v -> [%array1.fold_left.complex64.c Complex.add Complex.zero v])
    };
  per "int8_signed"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int8_signed
    ; sum_f = (fun v -> [%array1.fold_left.int8_signed.fortran (+) 0 v])
    ; sum_c = (fun v -> [%array1.fold_left.int8_signed.c (+) 0 v])
    };
  per "int8_unsigned"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int8_unsigned
    ; sum_f = (fun v -> [%array1.fold_left.int8_unsigned.fortran (+) 0 v])
    ; sum_c = (fun v -> [%array1.fold_left.int8_unsigned.c (+) 0 v])
    };
  per "int16_signed"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int16_signed
    ; sum_f = (fun v -> [%array1.fold_left.int16_signed.fortran (+) 0 v])
    ; sum_c = (fun v -> [%array1.fold_left.int16_signed.c (+) 0 v])
    };
  per "int16_unsigned"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int16_unsigned
    ; sum_f = (fun v -> [%array1.fold_left.int16_unsigned.fortran (+) 0 v])
    ; sum_c = (fun v -> [%array1.fold_left.int16_unsigned.c (+) 0 v])
    };
  per "int"
    { sum_n = (fun (v : int array) -> Array.fold_left (+) 0 v)
    ; kind  = Int
    ; sum_f = (fun v -> [%array1.fold_left.int.fortran (+) 0 v])
    ; sum_c = (fun v -> [%array1.fold_left.int.c (+) 0 v])
    };
  per "int32"
    { sum_n = (fun (v : int32 array) -> Array.fold_left Int32.add 0l v)
    ; kind  = Int32
    ; sum_f = (fun v -> [%array1.fold_left.int32.fortran Int32.add 0l v])
    ; sum_c = (fun v -> [%array1.fold_left.int32.c Int32.add 0l v])
    };
  per "int64"
    { sum_n = (fun (v : int64 array) -> Array.fold_left Int64.add 0L v)
    ; kind  = Int64
    ; sum_f = (fun v -> [%array1.fold_left.int64.fortran Int64.add 0L v])
    ; sum_c = (fun v -> [%array1.fold_left.int64.c Int64.add 0L v])
    };
  per "nativeint"
    { sum_n = (fun (v : Nativeint.t array) -> Array.fold_left Nativeint.add 0n v)
    ; kind  = Nativeint
    ; sum_f = (fun v -> [%array1.fold_left.nativeint.fortran Nativeint.add 0n v])
    ; sum_c = (fun v -> [%array1.fold_left.nativeint.c Nativeint.add 0n v])
    };
 let clsl a b = (int_of_char a) lsl (int_of_char b) |> Char.chr in
 per "char"
    { sum_n = (fun (v : char array) -> Array.fold_left clsl '\000' v)
    ; kind  = Char
    ; sum_f = (fun v -> [%array1.fold_left.char.fortran clsl '\000' v])
    ; sum_c = (fun v -> [%array1.fold_left.char.c clsl '\000' v])
    }
