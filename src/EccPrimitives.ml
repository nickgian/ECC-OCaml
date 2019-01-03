exception Error

type octet = string

let integer_of_octet oct =
  Z.of_string (String.concat "" ["0x"; oct])

type point = Infinity | Point of Z.t * Z.t ;;

(* assumes q : prime WON'T WORK WITH Binary fields *)
let of_octet octstr q a b = 
  if octstr = "00" then Infinity
  else
    begin
      let num_octets = (String.length octstr)/2 in
      let len = String.length @@ Z.format "%b" q in
      let octets = Z.(div (~$ len) (~$ 8)) in
      let tmp = Z.((~$ 2) * octets) in
        if Z.(tmp + one) = Z.(of_int num_octets) then
          let w = String.sub octstr 0 2 in
          let x = String.sub octstr 2 (Z.to_int tmp) in
          let y = String.sub octstr ((Z.to_int tmp) + 1) (Z.to_int tmp) in
            if w = "04" then
              let x_p = integer_of_octet x in
              let y_p = integer_of_octet y in
              let v1 = Z.((x_p ** 2) mod q) in
              let v2 = Z.(((x_p ** 3) + (a * x_p) + b) mod q) in
                if v1 = v2 then Point (x_p, y_p)
                else raise Error
            else raise Error
        else raise Error
    end

let () = Random.self_init ()
       
let inverse (a : Z.t) (p : Z.t) =
  Z.(invert a p) 

let int_pow a b = truncate ((float_of_int a) ** (float_of_int b))  


(*Generating random ints with a maximum length of decimal numbers*)
let rec random_big_int bound =
  let max_size = String.length (Z.to_string bound) in
  let a = max (max_size/4) 1 in 
  let size = a + Random.int (max_size - a) in
  let big_int = Bytes.create size in
  let rand_str = 
    Bytes.map (fun c -> let i = 48 + (Random.int 9) in Char.chr i) big_int 
  in
  let num = Z.(one + of_string(Bytes.to_string rand_str)) in
    match num < bound with
      | true -> num
      | false -> random_big_int bound

let verify_range (num : Z.t) (lowbound : Z.t) (upbound : Z.t) =
  (Z.(num >= lowbound) && Z.(num <= upbound))

module type FIELD = 
sig
  type curve
  (** The eliptic curve type *) 

  val lookup_curve : string -> curve
  (** Returns the specified curve *)

  val list_curves : unit -> string list
  (** Returns a list with the available curves *)                          

  val get_field : curve -> Z.t 
  (** Returns an integer specifying the finite field (i.e. p for the prime field
      Fp) *)                       
  val get_g : curve -> point 
  (** Returns the base point *)
  val get_n : curve -> Z.t
  (** Returns the order of the base point *)

  val get_a : curve -> Z.t

  val get_b : curve -> Z.t    

  val is_point : point -> curve -> bool 
  (** Check if a point lies on an eliptic curve *)
  val double_point : point -> curve -> point 
  (** Doubles a given point on a given eliptic curve *)
  val add_point : point -> point -> curve -> point
  (** Adds two given points on a given eliptic curve *)
  val multiply_point : point -> Z.t -> curve -> point 
  (** Scalar multiplication of a point and an integer on an elliptic curve *)
end

module PrimeField : FIELD =
struct

  type curve = {
    p : Z.t;
    a : Z.t;
    b : Z.t;
    g : point;
    n : Z.t;
    h : Z.t
  }  

  let curves = Hashtbl.create 13

  let lookup_curve name = Hashtbl.find curves name

  let list_curves () = 
    Hashtbl.fold (fun k _ acc -> k :: acc) curves []

  let get_field curve = curve.p

  let get_g curve = curve.g

  let get_n curve = curve.n

  let get_a curve = curve.a

  let get_b curve = curve.b                   


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
        let a = Z.((y ** 2) mod curve.p) in
        let b = Z.(((x ** 3) + (curve.a * x) + curve.b) mod curve.p) in
          a = b

  let double_point (r : point) curve =
    if not (is_point r curve) then raise Error;
    let p = curve.p in
      match r with
        | Infinity -> Infinity
        | Point (x,y) when y = Z.zero -> Infinity
        | Point (x, y) ->
          let a = Z.(((((~$ 3) * (x ** 2))) + curve.a) mod p) in
          let b = Z.((inverse (y + y) p) mod p) in
          let s = Z.(a * b mod p) in
          let x_r = Z.(((s ** 2) - (x + x)) mod p) in
          let y_r = Z.((-y + (s * (x - x_r))) mod p) in
            normalize (Point (x_r, y_r)) curve

  let add_point (r1 : point) (r2 : point) curve =
    if not ((is_point r1 curve) && (is_point r2 curve)) then raise Error;
    let p = curve.p in
      match r1, r2 with
        | _, Infinity -> r1
        | Infinity, _ -> r2
        | Point (x1, y1), Point (x2, y2) when x1 = x2 -> Infinity
        | r1, r2 when r1 = r2 -> double_point r1 curve
        | Point (x1, y1), Point (x2, y2) ->
          let s = Z.(((y2 - y1) * (inverse (x2 - x1) p)) mod p) in
          let xf = Z.(((s ** 2) - x1 - x2) mod p) in
          let yf = Z.((s * (x1 - xf) - y1) mod p) in
            normalize (Point (xf, yf)) curve

  (* Point multiplication is implemented using the double-and-add algorithm *)
  (*let multiply_point (q : point) (d : Z.t) curve =
    let d_bits = Z.format "%b" d in
    let r = ref q in
      String.iter (fun di -> r := double_point (!r) curve;
                             match di with
                               | '0' -> ()
                               | '1' -> r := add_point (!r) (q) curve
                               | _ -> failwith "Not a bit") d_bits;
      (!r)*)

  (* Point multiplication is implemented using montogomery ladders*)
  let multiply_point q d curve =
    let d_bits = Z.format "%b" d in
    let r_0 = ref Infinity in
    let r_1 = ref q in
      String.iter (fun di ->
          if di = '0' then
            begin
              r_1 := add_point !r_0 !r_1 curve;
              r_0 := double_point !r_0 curve
            end
          else
            begin
              r_0 := add_point !r_0 !r_1 curve;
              r_1 := double_point !r_1 curve
            end) d_bits;
      (!r_0)

  (* ECC data representation functions*)


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

  let test_curve = {p = Z.(~$ 23); 
                    a = Z.(~$ 9);
                    b = Z.(~$ 17); 
                    g = Point (Z.(~$ 9), Z.(~$ 5)); 
                    n = Z.(~$ 22); 
                    h = Z.(~$ 0);};;

  let () = 
    Hashtbl.add curves "brainpool_P256_r1" brainpool_P256_r1;
    Hashtbl.add curves "test_curve" test_curve;

end;;

