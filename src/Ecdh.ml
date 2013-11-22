open Ecc

module Ecdh =
  functor (F: FIELD) ->
    struct 
    
      (** Check that the public key is a valid public key*)
      let validate_pkey pk curve =
        match pk with
          | Infinity -> false
          | Point (pk_x, pk_y) -> 
              let curve_p = F.get_field curve in
            if (verify_range pk_x Z.zero Z.(curve_p - one)) &&
               (verify_range pk_y Z.zero Z.(curve_p - one)) &&
               F.is_point pk curve && 
               (F.multiply_point pk (F.get_n curve) curve) = Infinity
            then
              true
            else false

      (*Create public and secret key*)
      let rec create_keys curve = 
        let sk = random_big_int (F.get_n curve) in
        let pk = F.multiply_point (F.get_g curve) sk curve in
          if (validate_pkey pk curve) then
            (pk, sk)
          else
            create_keys curve

   (** Creates a session key from the the party's public key and the user's 
     * secret key *)
      
      let create_session_key pk sk curve = 
        let common = F.multiply_point pk sk curve in
          match common with 
            | Infinity -> raise Error
            | Point(x, _) -> 
                Sha1.to_hex (Sha1.string (Z.to_string x))
    
    end;;


