
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
