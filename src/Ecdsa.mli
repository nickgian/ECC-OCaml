module Ecdsa :
  functor (F : EccPrimitives.FIELD) ->
    sig
      val sign : string -> Z.t -> F.curve -> Z.t * Z.t
      val verify :
        string -> Z.t * Z.t -> EccPrimitives.point -> F.curve -> bool
    end
