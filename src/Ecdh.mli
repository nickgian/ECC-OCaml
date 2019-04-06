open EccPrimitives

module Ecdh :
  functor (C : Curve) ->
  sig
    val validate_pkey : point -> bool
    val create_keys : unit -> point * Z.t
    val create_session_key : point -> Z.t -> string
    val pubkey_of_seckey : Z.t -> point
  end