exception Invalid_point
exception Error

type point = Infinity | Point of Z.t * Z.t
type octet = string

val point_to_string : point -> string
val integer_of_octet : string -> Z.t
val of_octet : string -> Z.t -> Z.t -> Z.t -> point
val inverse : Z.t -> Z.t -> Z.t
val int_pow : int -> int -> int
val neg_mod : Z.t -> Z.t -> Z.t
val random_big_int : Z.t -> Z.t
val verify_range : Z.t -> Z.t -> Z.t -> bool

val hexstring_to_string : string -> string
val hexstring_of_string : string -> string

val string_of_point_uncompressed : point -> string
val string_of_point_compressed : point -> string

val bytes_of_point_uncompressed  : point -> string
val bytes_of_point_compressed : point -> string

module type Specs =
sig
  type t
  val curve : t
  val field : Z.t
  val g : point
  val n : Z.t
  val a : Z.t
  val b : Z.t
  val points : point list
end

module type Curve =
sig
  val get_field : Z.t
  val get_g : point
  val get_n : Z.t
  val get_a : Z.t
  val get_b : Z.t
  val get_points : point list
  val is_point : point -> bool
  val inverse_point : point -> point
  val double_point : point -> point
  val add_point : point -> point -> point
  val double_and_add : point -> Z.t -> point
  val montogomery_ladders : point -> Z.t -> point
  val multiply_point : point -> Z.t -> point
  val multiscalar_mul : point -> Z.t list -> point list -> point
end

module Make_Curve : functor (S : Specs) -> Curve
