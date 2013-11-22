module Ecdh :
  functor (F : EccPrimitives.FIELD) ->
    sig
      val validate_pkey : EccPrimitives.point -> F.curve -> bool
      val create_keys : F.curve -> EccPrimitives.point * Z.t
      val create_session_key :
        EccPrimitives.point -> Z.t -> F.curve -> string
    end
