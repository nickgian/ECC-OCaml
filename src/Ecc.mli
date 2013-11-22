exception Error
type point = Infinity | Point of Z.t * Z.t
val inverse : Z.t -> Z.t -> Z.t
val int_pow : int -> int -> int
val integer_of_octet : string -> int
val octList_of_octStr : string -> string list
val integer_of_octStr : string -> Z.t
val random_big_int : Z.t -> Z.t
val verify_range : Z.t -> Z.t -> Z.t -> bool
module type FIELD =
  sig
    type curve
    val lookup_curve : string -> curve
    val list_curves : unit -> string list
    val get_field : curve -> Z.t
    val get_g : curve -> point
    val get_n : curve -> Z.t
    val is_point : point -> curve -> bool
    val double_point : point -> curve -> point
    val add_point : point -> point -> curve -> point
    val multiply_point : point -> Z.t -> curve -> point
  end
module PrimeField : FIELD
