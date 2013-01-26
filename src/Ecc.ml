module Ecc =
struct
  type point = Infinity | Point of Z.t * Z.t
  type elliptic_curve = {
    p : Z.t;
    a : Z.t;
    b : Z.t;
    g : point;
    n : Z.t;
    h : Z.t
  }


  (* Modular Arithmetic*)

  let inverse (a : Z.t) (p : Z.t) =
    Z.(invert a p)

  (*Elliptic Curve Functions*)

  let normalize (r : point) curve =
    let p = curve.p in
    let normalize_aux (x : Z.t) =
      match x with
        | x when Z.(x < zero) -> Z.(p + x)
        | x -> x
    in
      match r with
        | Infinity -> Infinity
        | Point (x_r, y_r) -> Point (normalize_aux x_r, normalize_aux y_r) 

  let is_point (r : point) curve =
    match r with
      | Infinity -> true
      | Point (x, y) ->
          Z.((y ** 2) mod curve.p) = Z.(((x ** 3) + (curve.a * x) + curve.b) mod curve.p)

  let double_point (ec_point : point) curve =
    let p = curve.p in
      match ec_point with
        | Infinity -> Infinity
        | Point (x, y) ->
            (match Z.(zero = y) with
               | true -> Infinity
               | false -> 
                   let s = Z.(((((~$ 3) * (x ** 2)) + curve.a) * (inverse ((~$ 2) * y) p)) mod p) in
                   let x_r = Z.(((s ** 2) - ((~$ 2) * x)) mod p) in
                   let y_r = Z.((-y + (s * (x - x_r))) mod p) in
                     normalize (Point (x_r, y_r)) curve)

  let add_point (q : point) (r : point) curve =
    let p = curve.p in
      match q,r with
        | Infinity, Infinity -> Infinity
        | _, Infinity -> q
        | Infinity, _ -> r
        | Point (x_q, y_q), Point (x_r, y_r) -> 
            let s = Z.(((y_q - y_r) * (inverse (x_q - x_r) p)) mod p) in
            let x_f = Z.(((s ** 2) - x_q - x_r) mod p) in
            let y_f = Z.((s * (x_q - x_f) - y_q) mod p) in
              match (y_f = Z.zero) with
                | true -> Infinity
                | false -> normalize (Point (x_f, y_f)) curve

  (*Point multiplication using double-and-add method*)
  (*let multiply_point (q : point) (d : Z.t) (curve : elliptic_curve) =
   let rec multiply_aux d r =
   match (d = Z.zero) || (d = Z.one) with
   | true -> r
   | false ->
   let t = double_point r curve in
   ( 
   match ((Z.logand d (Z.one)) = Z.one) with
   | true -> multiply_aux (Z.(d asr 1)) (add_point t q curve)
   | false -> multiply_aux (Z.(d asr 1)) t 
   )
   in
   multiply_aux d q
   *)

  let multiply_point (q : point) (d : Z.t) (curve : elliptic_curve) =
    let d_binary = Z.format "%b" d in
    let d_bits = String.sub d_binary 1 ((String.length d_binary) - 1) in
    let r = ref q in
      String.iter (fun di -> r := double_point (!r) curve;
                             match di with
                               | '0' -> ()
                               | '1' -> r := add_point (!r) (q) curve
                               | _ -> failwith "Not a bit") d_bits;
      (!r)

  (* ECC data representation functions*)

  let int_pow a b = truncate ((float_of_int a) ** (float_of_int b))  

  let integer_of_octet oct =
    int_of_string (String.concat "" ["0x"; oct])

  let octList_of_octStr str =
    let str_len = String.length str in
    let rec aux i acc =
      match i with
        | -2 -> acc
        | i -> aux (i - 2) ((String.sub str i 2) :: acc)
    in
      aux (str_len - 2) []

  let integer_of_octStr str =
    let oct_list = octList_of_octStr str in
    let m_len = (String.length str) / 2 in
    let rec aux m i sum =
      match m with
        | [] -> sum
        | m_i :: t -> 
            let tmp = (int_pow 2 (8 * (m_len - 1 - i))) * (integer_of_octet m_i) in
              aux t (i + 1) (Z.((~$ tmp) + sum))
    in
      aux oct_list 0 Z.zero

  (* Recommended Elliptic Curve Domain Parameters*)

  (* http://www.ecc-brainpool.org/download/Domain-parameters.pdf *)
  let brainpool_P256_r1 =
    {
      p = Z.of_string_base 16 "A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377";
      a = Z.of_string_base 16 "7D5A0975FC2C3057EEF67530417AFFE7FB8055C126DC5C6CE94A4B44F330B5D9";
      b = Z.of_string_base 16 "26DC5C6CE94A4B44F330B5D9BBD77CBF958416295CF7E1CE6BCCDC18FF8C07B6";
      g = Point (Z.of_string_base 16 "8BD2AEB9CB7E57CB2C4B482FFC81B7AFB9DE27E1E3BD23C23A4453BD9ACE3262",
                 Z.of_string_base 16 "547EF835C3DAC4FD97F8461A14611DC9C27745132DED8E545C1D54C72F046997");
      n = Z.of_string_base 16 "A9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7";
      h = Z.one
    }

  let test_curve = {p = Z.(~$ 23); a = Z.(~$ 9); b = Z.(~$ 17); g = Point (Z.(~$ 9), Z.(~$ 5)); n = Z.(~$ 22); h = Z.(~$ 0);};;

  (*Generating random ints with a maximum length of decimal numbers*)
  let rec random_big_int bound =
    Random.self_init ();
    let max_size = String.length (Z.to_string bound) in
    let size = 1 + Random.int (max_size - 1) in
    let big_int = String.create size in
    let rand_str = String.map (fun c -> 
                                 let i = 48 + (Random.int 9) in Char.chr i) big_int in
    let num = Z.(one + (of_string rand_str)) in
      match num < bound with
        | true -> num
        | false -> random_big_int bound

  (* ECDSA*)

  let rec sign m sk curve =
    let k = random_big_int curve.n in
      match (try Some (Z.invert k curve.n) with division_by_zero -> None) with
        | None -> sign m sk curve
        | Some inv_k ->
            let kG = multiply_point (curve.g) k curve in
              (match kG with 
                 | Infinity -> sign m sk curve
                 | Point (x1, y1) ->
                     let r = Z.(x1 mod curve.n) in
                       (match (r = Z.zero) with
                          | true -> sign m sk curve
                          | false -> 
                              let hash_m = Digest.to_hex (Digest.string m) in
                              let e = Z.of_string_base 16 hash_m in
                              let s = Z.((inv_k * (e + sk * r)) mod (curve.n)) in
                                (match (s = Z.zero) with
                                   | true -> sign m sk curve
                                   | false -> 
                                       (match Z.((gcd s curve.n) = Z.one) with
                                          | true -> (r, s)
                                          | false -> sign m sk curve
                                       ))))

  let verify_range (num : Z.t) (lowbound : Z.t) (upbound : Z.t) =
    (Z.(num >= lowbound) && Z.(num <= upbound))

  let verify m (r, s) pk curve =
    match (verify_range r Z.one Z.(curve.n - one), verify_range s Z.one Z.(curve.n - one)) with
      | (true, true) ->
          let hash_m = Digest.to_hex (Digest.string m) in
          let e = Z.of_string_base 16 hash_m in
          let w = Z.invert s curve.n in
          let u1 = Z.((e*w) mod curve.n) in 
          let u2 = Z.((r*w) mod curve.n) in
          let u1G = multiply_point curve.g u1 curve in
          let u2Q = multiply_point pk u2 curve in
          let x = add_point u1G u2Q curve in
            (match x with
               | Infinity -> false
               | Point (x1, y1) -> Z.((x1 mod curve.n) = r))
      | (_, _) -> false


(*Create public and secret key*)

let rec create_keys curve = 
  let sk = random_big_int curve.n in
  let pk = multiply_point (curve.g) sk curve in
    match pk with
      | Infinity -> failwith "infinity\n"
      | Point (pk_x, pk_y) -> 
          begin
            match is_point (Point (pk_x, pk_y)) curve with
              | true -> 
                  (Point (pk_x, pk_y), sk)
              | false -> 
                  failwith "false\n"
          end
end;;



