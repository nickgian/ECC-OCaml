exception Error
type octet = string
val integer_of_octet : string -> Z.t
type point = Infinity | Point of Z.t * Z.t

val string_of_point_uncompressed : point -> string
val string_of_point_compressed   : point -> string

val bytes_of_point_uncompressed  : point -> string
val bytes_of_point_compressed    : point -> string

val hexstring_to_string          : string -> string
val hexstring_of_string          : string -> string


val of_octet : string -> Z.t -> Z.t -> Z.t -> point
val inverse : Z.t -> Z.t -> Z.t
val int_pow : int -> int -> int
val random_big_int : Z.t -> Z.t
val verify_range : Z.t -> Z.t -> Z.t -> bool
module type FIELD =
  sig
    type curve
    (** The eliptic curve type *) 
    val lookup_curve : string -> curve
    (** Returns the specified curve *)

    val list_curves : unit -> string list
    (** Returns a list with the available curves *)  

    val get_field : curve -> Z.t
    (** Returns an integer specifying the finite field (i.e. p for
        the prime field Fp) *) 
                               
    val get_g : curve -> point
    (** Returns the base point *)

    val get_n : curve -> Z.t
    (** Returns the order of the base point *)  
                           
    val get_a : curve -> Z.t
    val get_b : curve -> Z.t

    val is_point : point -> curve -> bool
    (** Check if a point lies on an eliptic curve *)
                                       
    val double_point : point -> curve -> point
    (** Doubles a given point on a given eliptic curve *)

    val add_point : point -> point -> curve -> point
    (** Adds two given points on a given eliptic curve *)

    val multiply_point : point -> Z.t -> curve -> point
    (** Scalar multiplication of a point and an integer on an elliptic curve *)

  end
module PrimeField : FIELD
