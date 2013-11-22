open EccPrimitives


module Ecdsa = 
  functor (F : FIELD) ->
    struct 

    let rec sign m sk curve =
      let curve_n = F.get_n curve in
      let curve_g = F.get_g curve in
      let k = random_big_int curve_n in (* do we need to check that k is invertible ? *)
        match F.multiply_point (curve_g) k curve with
          | Infinity -> sign m sk curve 
          | Point (x1, y1) ->
              let r = Z.(x1 mod curve_n) in
                if r = Z.zero then sign m sk curve 
                else 
                  let inv_k = (Z.invert k curve_n) in
                  let hash_m = Sha1.to_hex (Sha1.string m) in
                  let e = Z.of_string_base 16 hash_m in
                  let s = Z.((inv_k * (e + sk * r)) mod (curve_n)) in
                    if s = Z.zero then sign m sk curve 
                    else (r, s)

    let verify m (r, s) pk curve =
      let curve_n = F.get_n curve in
      let curve_g = F.get_g curve in
      let upper = Z.(curve_n - one) in
        match (verify_range r Z.one upper, verify_range r Z.one upper) with
          | (true, true) -> 
              let hash_m = Sha1.to_hex (Sha1.string m) in
              let e = Z.of_string_base 16 hash_m in
              let w = Z.invert s curve_n in
              let u1 = Z.((e*w) mod curve_n) in 
              let u2 = Z.((r*w) mod curve_n) in
              let u1G = F.multiply_point curve_g u1 curve in
              let u2Q = F.multiply_point pk u2 curve in
              let x = F.add_point u1G u2Q curve in
                (match x with
                   | Infinity -> false
                   | Point (x1, y1) -> Z.((x1 mod curve_n) = r))
          | (_, _) -> false
    end;;
