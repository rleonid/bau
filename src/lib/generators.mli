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

open BigarrayExt

(** [int_kind k] is true for [Bigarray.kind] types that represent integers. *)
val int_kind : ('a, 'b) kind -> bool

(** [default_bound kind] is the default upper bound value used in generators.
    For [int_kind]'s this value is enforced. *)
val default_bound : ('a, 'b) kind -> 'a

(** [random ?bound kind] returns a function to generate new random values of
    [kind] bound above by [bound] *)
val random : ?bound:'a -> ('a, 'b) kind -> (unit -> 'a)

(** [array1 bound kind layout length] creates a new random [Array1]. *)
val array1 : ?bound:'a -> ('a, 'b) kind -> 'c layout -> int -> ('a, 'b, 'c) Array1.t

(** [array2 bound kind layout rows columns] creates a new random [Array2]. *)
val array2 : ?bound:'a -> ('a, 'b) kind -> 'c layout -> int -> int -> ('a, 'b, 'c) Array2.t

(** [array3 bound kind layout heigth width depth] creates a new random [Array3]. *)
val array3 : ?bound:'a -> ('a, 'b) kind -> 'c layout -> int -> int -> int -> ('a, 'b, 'c) Array3.t

(** [general bound kind layout dimensions] creates a new random [Genarray]. *)
val general : ?bound:'a -> ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) Genarray.t
