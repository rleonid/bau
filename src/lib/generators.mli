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

(** [int_kind k] is true for [Bigarray.kind] types that represent integers. *)
val int_kind : ('a, 'b) Bigarrayo.kind -> bool                                                                                                                                       

(** [default_bound kind] is the default upper bound value used in generators.
    For [int_kind]'s this value is enforced. *)
val default_bound : ('a, 'b) Bigarrayo.kind -> 'a

(** [random ?bound kind] returns a function to generate new random values of
    [kind] bound above by [bound] *)
val random : ?bound:'a -> ('a, 'b) Bigarrayo.kind -> (unit -> 'a)

(** [array1 bound kind layout length] creates a new random [Array1]. *)
val array1 : ?bound:'a -> ('a, 'b) Bigarrayo.kind -> 'c Bigarrayo.layout -> int -> ('a, 'b, 'c) Bigarrayo.A1.t

(** [array2 bound kind layout rows columns] creates a new random [Array2]. *)
val array2 : ?bound:'a -> ('a, 'b) Bigarrayo.kind -> 'c Bigarrayo.layout -> int -> int -> ('a, 'b, 'c) Bigarrayo.A2.t

(** [array3 bound kind layout heigth width depth] creates a new random [Array3]. *)
val array3 : ?bound:'a -> ('a, 'b) Bigarrayo.kind -> 'c Bigarrayo.layout -> int -> int -> int -> ('a, 'b, 'c) Bigarrayo.A3.t

(** [general bound kind layout dimensions] creates a new random [Genarray]. *)
val general : ?bound:'a -> ('a, 'b) Bigarrayo.kind -> 'c Bigarrayo.layout -> int array -> ('a, 'b, 'c) Bigarrayo.GA.t
