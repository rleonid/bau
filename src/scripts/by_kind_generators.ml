
open Bigarrayo
    
let ran_gen : type a b. (a, b) kind -> (unit -> a) = function
  | Float32 -> (fun () -> Random.float 1.0)
  | Float64 -> (fun () -> Random.float 1.0)
  | Complex32 -> fun () -> { Complex.re = Random.float 1.; im = Random.float 1.}
  | Complex64 -> fun () -> { Complex.re = Random.float 1.; im = Random.float 1.}
  | Int8_signed -> fun () -> (Random.int 256) - 256
  | Int8_unsigned -> fun () -> Random.int 256
  | Int16_signed -> fun () -> (Random.int 65536) - 65536
  | Int16_unsigned -> fun () -> Random.int 65536
  | Int -> fun () -> Random.int 1073741823
  | Nativeint -> fun () -> Random.nativeint Nativeint.max_int
  | Int32 -> fun () -> Random.int32 Int32.max_int
  | Int64 -> fun () -> Random.int64 Int64.max_int
  | Char -> fun () -> char_of_int (Random.int 256)

let vec_gen k l n =
  let re = ran_gen k in
  A1.init k l n (fun _ -> re ())
  
let all_vecs = vec_gen Float32 Fortran_layout 10
             , vec_gen Float64 Fortran_layout 10
             , vec_gen Complex32 Fortran_layout 10
             , vec_gen Complex64 Fortran_layout 10
             , vec_gen Int8_signed Fortran_layout 10
             , vec_gen Int8_unsigned Fortran_layout 10
             , vec_gen Int16_signed Fortran_layout 10
             , vec_gen Int16_unsigned Fortran_layout 10
             , vec_gen Int Fortran_layout 10
             , vec_gen Nativeint Fortran_layout 10
             , vec_gen Int32 Fortran_layout 10
             , vec_gen Int64 Fortran_layout 10
             , vec_gen Char Fortran_layout 10

let mat_gen k l n m =
  let re = ran_gen k in
  A2.init k l n m (fun _ _ -> re ())
  
let all_mats = mat_gen Float32 Fortran_layout 10 10
             , mat_gen Float64 Fortran_layout 10 10
             , mat_gen Complex32 Fortran_layout 10 10
             , mat_gen Complex64 Fortran_layout 10 10
             , mat_gen Int8_signed Fortran_layout 10 10
             , mat_gen Int8_unsigned Fortran_layout 10 10
             , mat_gen Int16_signed Fortran_layout 10 10
             , mat_gen Int16_unsigned Fortran_layout 10 10
             , mat_gen Int Fortran_layout 10 10
             , mat_gen Nativeint Fortran_layout 10 10
             , mat_gen Int32 Fortran_layout 10 10
             , mat_gen Int64 Fortran_layout 10 10
             , mat_gen Char Fortran_layout 10 10

let ar3_gen k l n m o =
  let re = ran_gen k in
  A3.init k l n m o (fun _ _ _ -> re ())
  
let all_ar3s = ar3_gen Float32 Fortran_layout 10 10 10
             , ar3_gen Float64 Fortran_layout 10 10 10
             , ar3_gen Complex32 Fortran_layout 10 10 10
             , ar3_gen Complex64 Fortran_layout 10 10 10
             , ar3_gen Int8_signed Fortran_layout 10 10 10
             , ar3_gen Int8_unsigned Fortran_layout 10 10 10
             , ar3_gen Int16_signed Fortran_layout 10 10 10
             , ar3_gen Int16_unsigned Fortran_layout 10 10 10
             , ar3_gen Int Fortran_layout 10 10 10
             , ar3_gen Nativeint Fortran_layout 10 10 10
             , ar3_gen Int32 Fortran_layout 10 10 10
             , ar3_gen Int64 Fortran_layout 10 10 10
             , ar3_gen Char Fortran_layout 10 10 10

let gen_gen k l dims =
  let re = ran_gen k in
  GA.init k l dims (fun _ -> re ())

