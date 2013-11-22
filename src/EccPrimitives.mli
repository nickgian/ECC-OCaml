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
