open EccPrimitives

module Ecdsa = 
  functor (C : Curve) ->
  struct 
    open C

    let rec sign m sk =
      let curve_n = get_n in
      let curve_g = get_g in
      let k = random_big_int curve_n in (* do we need to check that k is invertible ? *)
      match multiply_point curve_g k with
      | Infinity -> sign m sk 
      | Point (x1, _) ->
        let r = Z.(x1 mod curve_n) in
        if r = Z.zero then sign m sk 
        else 
          let inv_k = (Z.invert k curve_n) in
          let hash_m = Sha1.to_hex (Sha1.string m) in
          let e = Z.of_string_base 16 hash_m in
          let s = Z.((inv_k * (e + sk * r)) mod (curve_n)) in
          if s = Z.zero then sign m sk 
          else (r, s)

    let verify m (r, s) pk =
      let curve_n = get_n in
      let curve_g = get_g in
      let upper = Z.(curve_n - one) in
      match (verify_range r Z.one upper, verify_range r Z.one upper) with
      | (true, true) -> 
        let hash_m = Sha1.to_hex (Sha1.string m) in
        let e = Z.of_string_base 16 hash_m in
        let w = Z.invert s curve_n in
        let u1 = Z.((e*w) mod curve_n) in 
        let u2 = Z.((r*w) mod curve_n) in
        let u1G = multiply_point curve_g u1 in
        let u2Q = multiply_point pk u2 in
        let x = add_point u1G u2Q in
        (match x with
         | Infinity -> false
         | Point (x1, _) -> Z.((x1 mod curve_n) = r))
      | (_, _) -> false
  end
