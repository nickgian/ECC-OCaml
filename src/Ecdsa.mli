open EccPrimitives

module Ecdsa :
  functor (C : Curve) ->
  sig
    val sign : string -> Z.t -> Z.t * Z.t
    val verify : string -> Z.t * Z.t -> point -> bool
  end