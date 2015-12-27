
open Bigarray

(* The default upper bounds for the random generators of each kind. *)
let default_bound : type a b. (a, b) kind -> a = function
  | Float32         -> 1.0
  | Float64         -> 1.0
  | Complex32       -> { Complex.re = 1.; im = 1.}
  | Complex64       -> { Complex.re = 1.; im = 1.}
  | Int8_signed     -> 256 / 2 
  | Int8_unsigned   -> 256
  | Int16_signed    -> 65536 / 2
  | Int16_unsigned  -> 65536
  | Int             -> 1073741823
  | Nativeint       -> Nativeint.max_int
  | Int32           -> Int32.max_int
  | Int64           -> Int64.max_int
  | Char            -> char_of_int 256

(* Create a random generator.
   The values are bound by the [zero] of each kind and [bound] above. *)
let random : type a b. ?bound:a -> (a, b) kind -> (unit -> a) =
  fun ?bound kind ->
    let def_b = default_bound kind in
    let bound = match bound with | None -> def_b | Some b -> b in
    match kind with
    | Float32         -> fun () -> Random.float bound
    | Float64         -> fun () -> Random.float bound
    | Complex32       -> let br = bound.Complex.re in
                         let bi = bound.Complex.im in
                         fun () -> { Complex.re = Random.float br
                                   ; Complex.im = Random.float bi}
    | Complex64       -> let br = bound.Complex.re in
                         let bi = bound.Complex.im in
                         fun () -> { Complex.re = Random.float br
                                   ; Complex.im = Random.float bi}
    | Int8_signed     -> let b = min def_b bound in
                         fun () -> Random.int b
    | Int8_unsigned   -> fun () -> Random.int bound
    | Int16_signed    -> let b = min def_b bound in
                         fun () -> Random.int b
    | Int16_unsigned  -> fun () -> Random.int bound
    | Int             -> fun () -> Random.int bound
    | Nativeint       -> fun () -> Random.nativeint bound
    | Int32           -> fun () -> Random.int32 bound
    | Int64           -> fun () -> Random.int64 bound
    | Char            -> let c = int_of_char bound in
                         fun () -> char_of_int (Random.int c)

