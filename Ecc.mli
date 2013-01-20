module Ecc :
  sig
    type point = Infinity | Point of Z.t * Z.t
    type elliptic_curve = {
      p : Z.t;
      a : Z.t;
      b : Z.t;
      g : point;
      n : Z.t;
      h : Z.t;
    }
    val is_point : point -> elliptic_curve -> bool
    val double_point : point -> elliptic_curve -> point
    val add_point : point -> point -> elliptic_curve -> point
    val multiply_point : point -> Z.t -> elliptic_curve -> point
    val int_pow : int -> int -> int
    val integer_of_octet : string -> int
    val octList_of_octStr : string -> string list
    val integer_of_octStr : string -> Z.t
    val sec_256_r1 : elliptic_curve
    val brainpool_P256_r1 : elliptic_curve
  end
