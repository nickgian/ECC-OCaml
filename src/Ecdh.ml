open EccPrimitives

module Ecdh =  
  functor (C: Curve) -> 
  struct 
    open C
    (* Check that the public key is a valid public key*)
    let validate_pkey pk =
      match pk with
      | Infinity -> false
      | Point (pk_x, pk_y) -> 
        let curve_p = get_field in
        if (verify_range pk_x Z.zero Z.(curve_p - one)) &&
           (verify_range pk_y Z.zero Z.(curve_p - one)) &&
           is_point pk && 
           (multiply_point pk get_n ) = Infinity
        then
          true
        else false

    (*Create public and secret key*)
    let rec create_keys()  = 
      let sk = random_big_int get_n in
      let pk = multiply_point get_g sk in
      if validate_pkey pk then
        (pk, sk)
      else
        create_keys()

    (* Creates a session key from the other party's public key and the user's 
      * secret key *)

    let create_session_key pk sk = 
      let common = multiply_point pk sk in
      match common with 
      | Infinity -> raise Error
      | Point(x, _) -> 
        Sha1.to_hex (Sha1.string (Z.to_string x))

    (* Creates public key from secret key *)
    let pubkey_of_seckey sk =
      multiply_point get_g sk
  end;;


