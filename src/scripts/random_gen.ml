
open BigarrayExt
open Generators
    
let all_vecs layout ds =
  array1 Float32 layout ds
  , array1 Float64 layout ds
  , array1 Complex32 layout ds
  , array1 Complex64 layout ds
  , array1 Int8_signed layout ds
  , array1 Int8_unsigned layout ds
  , array1 Int16_signed layout ds
  , array1 Int16_unsigned layout ds
  , array1 Int layout ds
  , array1 Nativeint layout ds
  , array1 Int32 layout ds
  , array1 Int64 layout ds
  , array1 Char layout ds

let all_mats layout ds =
  array2 Float32 layout ds ds
  , array2 Float64 layout ds ds
  , array2 Complex32 layout ds ds
  , array2 Complex64 layout ds ds
  , array2 Int8_signed layout ds ds
  , array2 Int8_unsigned layout ds ds
  , array2 Int16_signed layout ds ds
  , array2 Int16_unsigned layout ds ds
  , array2 Int layout ds ds
  , array2 Nativeint layout ds ds
  , array2 Int32 layout ds ds
  , array2 Int64 layout ds ds
  , array2 Char layout ds ds

let all_ar3s layout ds =
  array3 Float32 layout ds ds ds
  , array3 Float64 layout ds ds ds
  , array3 Complex32 layout ds ds ds
  , array3 Complex64 layout ds ds ds
  , array3 Int8_signed layout ds ds ds
  , array3 Int8_unsigned layout ds ds ds
  , array3 Int16_signed layout ds ds ds
  , array3 Int16_unsigned layout ds ds ds
  , array3 Int layout ds ds ds
  , array3 Nativeint layout ds ds ds
  , array3 Int32 layout ds ds ds
  , array3 Int64 layout ds ds ds
  , array3 Char layout ds ds ds

let all_gen layout dims =
  general Float32 layout dims
  , general Float64 layout dims
  , general Complex32 layout dims
  , general Complex64 layout dims
  , general Int8_signed layout dims
  , general Int8_unsigned layout dims
  , general Int16_signed layout dims
  , general Int16_unsigned layout dims
  , general Int layout dims
  , general Nativeint layout dims
  , general Int32 layout dims
  , general Int64 layout dims
  , general Char layout dims


