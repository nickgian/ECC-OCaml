open Printf 

exception Invalid_point
exception Error

type point = Infinity | Point of Z.t * Z.t
type octet = string

let point_to_string = function 
  | Infinity -> sprintf "0"
  | Point(x,y) -> 
    sprintf "Point(Z.of_string \"%s\", Z.of_string \"%s\")" 
      (Z.to_string x) (Z.to_string y)

let integer_of_octet oct =
  Z.of_string (String.concat "" ["0x"; oct])

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

let inverse (a : Z.t) (p : Z.t) =
  Z.(invert a p) 

let int_pow a b = 
  truncate ((float_of_int a) ** (float_of_int b))

let neg_mod a p =
  let r = Z.(a mod p) in 
  if Z.(r < zero) then
    Z.(r+p)
  else  
    r

(*Generating random ints with a maximum length of decimal numbers*)
let rec random_big_int bound =
  let () = Random.self_init () in
  let rec aux i acc = 
    if i = 0 then
      acc
    else 
      let b = Random.int 2 in
      aux (i-1) (acc ^ string_of_int b) 
  in
  let size = 
    String.length (Z.format "%b" bound)
  in
  let num = Z.of_string (aux size "0b") in
  match num < bound || num = Z.zero with
  | true -> num
  | false -> random_big_int bound 

let verify_range (num : Z.t) (lowbound : Z.t) (upbound : Z.t) =
  (Z.(num >= lowbound) && Z.(num <= upbound))


let charlist_to_string cl = 
  String.concat "" (List.map (String.make 1) cl)
let charlist_of_string s = 
  List.init (String.length s) (String.get s)

let hexchar_to_int c = match c with
  | '0'..'9' -> int_of_char c - 48
  | 'a'..'f' -> int_of_char c - 87
  | 'A'..'F' -> int_of_char c - 55 
  | _ -> raise Error

let hexchar_of_int i = match i>9 with
  | false -> char_of_int (i + 48)
  | true -> char_of_int (i + 87)

let hexstring_to_string s =
  let cl = charlist_of_string s in
  let rec go l =
    match l with
    | [] -> []
    | [_] -> []
    | x :: y :: xs -> char_of_int ((hexchar_to_int x * 16) + hexchar_to_int y) :: go xs
  in charlist_to_string (go cl) 

let hexstring_of_string s =
  let cl = charlist_of_string s in
  let rec go l =
    match l with
    | [] -> []
    | x :: xs -> hexchar_of_int (int_of_char x / 16) :: hexchar_of_int (int_of_char x mod 16) :: go xs
  in charlist_to_string (go cl)

let string_of_point_uncompressed p = match p with
  | Infinity -> "0400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  | Point (x, y) -> String.concat ""
                      ["04" ; Z.format "%x" x ; Z.format "%x" y]
(* test 
   18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725 ->
   0250863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352 ->
   0b7c28c9b7290c98d7438e70b3d3f7c848fbd7d1dc194ff83f4f7cc9b1378e98 ->
*)
let string_of_point_compressed p = match p with
  | Infinity -> "0200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  | Point (x, y) -> if (Z.rem y (Z.of_int 2)) = Z.zero
    then String.concat "" ["02" ; Z.format "%x" x]
    else String.concat "" ["03" ; Z.format "%x" x]


let bytes_of_point_uncompressed p = 
  hexstring_to_string (string_of_point_uncompressed p)
let bytes_of_point_compressed p = 
  hexstring_to_string (string_of_point_compressed p)

(** Auxiliary fuction for m*)
let mods k w =
  if Z.(k mod w) >= Z.( w / ~$2) then 
    Z.((k mod w) - w)
  else
    Z.(k mod w)

let w_naf k w = 
  let rec aux d acc =
    if d <= Z.zero then 
      acc
    else
      begin
        if Z.(d mod ~$2) = Z.one then
          let ki = mods d Z.(~$2 ** w) in
          aux Z.((d-ki) / ~$2) (ki :: acc)
        else
          aux Z.(d / ~$2) (Z.zero :: acc) 
      end 
  in 
  aux k []

let return_point p  pc_p  ki =
  if ki = Z.one || ki = Z.minus_one then 
    p 
  else if ki = Z.(~$3) || ki = Z.(- ~$3) then 
    pc_p.(0)
  else if ki = Z.(~$ 5) || ki = Z.(- ~$5) then 
    pc_p.(1)
  else if ki = Z.(~$ 7) || ki = Z.(- ~$7) then 
    pc_p.(2)
  else if ki = Z.(~$ 9) || ki = Z.(- ~$9) then 
    pc_p.(3)
  else if ki = Z.(~$ 11) || ki = Z.(- ~$11) then 
    pc_p.(4)
  else if ki = Z.(~$ 13) || ki = Z.(- ~$13) then 
    pc_p.(5)
  else 
    pc_p.(6)

module type Specs = sig 

  type t
  (** The eliptic curve type *)
  val curve : t
  (** The eliptic curve variable *)

  val field : Z.t 
  (** Returns an integer specifying the finite field (i.e. p for the prime field
      Fp) *)                       
  val g : point 
  (** Returns the base point *)
  val n : Z.t
  (** Returns the order of the base point *)
  val a : Z.t
  (** Returns the variable a of the eliptic curve equation *)
  val b : Z.t    
  (** Returns the variable b of the eliptic curve equation *)
  val points : point list
  (** Returns the point list used to pre-compute points to use on 
      multiply_point algorithm  *)

end

module type Curve = sig 

  val get_field : Z.t 
  (** Returns an integer specifying the finite field (i.e. p for the prime field
      Fp) *)                       
  val get_g : point 
  (** Returns the base point *)
  val get_n : Z.t
  (** Returns the order of the base point *)
  val get_a : Z.t
  (** Returns the variable a of the eliptic curve equation *)
  val get_b : Z.t    
  (** Returns the variable b of the eliptic curve equation *)
  val get_points : point list
  (** Returns the point list used to pre-compute points to use on 
      multiply_point algorithm  *)
  val is_point : point -> bool
  (** Check if a point lies on an eliptic curve *)
  val inverse_point : point -> point
  (** Given a point P returns the point -P *)
  val double_point : point -> point
  (** Doubles a given point on a given eliptic curve *)
  val add_point : point -> point -> point
  (** Adds two given points on a given eliptic curve *)
  val double_and_add : point -> Z.t -> point
  (** Point multiplication with the double-and-add algorithm *)
  val montogomery_ladders : point -> Z.t -> point 
  (** Point multiplication is implemented using montogomery ladders *)
  val multiply_point : point -> Z.t -> point
  (* Point multiplication is implemented using the 
     w-ary non-adjacent form (wNAF) algorithm *)
  val multiscalar_mul : 
    point -> Z.t list -> point list -> point 
    (** inner product of a scalar vector and a point vector *)

end 

module Make_Curve(S : Specs) : Curve =
struct
  open S

  let get_field = field
  let get_g = g
  let get_n = n 
  let get_a = a
  let get_b = b 
  let get_points = points
  let computed_points = Hashtbl.create 130

  (*Elliptic Curve Functions*)

  let normalize (r : point)  =
    let p = get_field in
    let rec normalize_aux (x : Z.t) =
      match x with
      | x when Z.(x < zero) -> normalize_aux Z.(p + x)
      | x -> x
    in
    match r with
    | Infinity -> Infinity
    | Point (x_r, y_r) -> 
      Point (normalize_aux x_r, normalize_aux y_r) 

  let is_point (r : point)=
    match r with
    | Infinity -> true
    | Point (x, y) ->
      let a = Z.((y ** 2) mod get_field) in
      let b = 
        Z.(((x ** 3) + (get_a * x) + get_b) mod get_field) in
      a = b

  let inverse_point point = 
    match point with
      Infinity -> Infinity
    | Point(x,y) -> normalize (Point(x, Z.(-y)))

  let double_point (r : point) =
    if not (is_point r) then raise Invalid_point;
    let p = get_field in
    match r with
    | Infinity -> Infinity
    | Point (x,y) when y = Z.zero -> Infinity
    | Point (x, y) ->
      let a = Z.(((((~$ 3) * (x ** 2))) + get_a) mod p) in
      let b = Z.((inverse (y + y) p) mod p) in
      let s = Z.(a * b mod p) in
      let x_r = Z.(((s ** 2) - (x + x)) mod p) in
      let y_r = Z.((-y + (s * (x - x_r))) mod p) in
      normalize (Point (x_r, y_r)) 

  let add_point (r1 : point) (r2 : point) =
    if not ((is_point r1) && (is_point r2)) then raise Invalid_point;
    let p = get_field in
    match r1, r2 with
    | _, Infinity -> r1
    | Infinity, _ -> r2
    | r1, r2 when r1 = r2 -> double_point r1
    | Point (x1, y1), Point (x2, y2) when x1 = x2 -> Infinity
    | Point (x1, y1), Point (x2, y2) ->
      let s = Z.(((y2 - y1) * (inverse (x2 - x1) p)) mod p) in
      let xf = Z.(((s ** 2) - x1 - x2) mod p) in
      let yf = Z.((s * (x1 - xf) - y1) mod p) in
      normalize (Point (xf, yf)) 

  let pre_computation (p : point) =
    let points = Array.make 7 Infinity in 
    let p2 = double_point p in
    let p3 = add_point p p2 in
    let p5 = add_point p2 p3 in
    let p7 = add_point p2 p5 in
    let p9 = add_point p2 p7 in
    let p11 = add_point p2 p9 in
    let p13 = add_point p2 p11 in
    let p15 = add_point p2 p13 in

    let () = 
      points.(0) <- p3;
      points.(1) <- p5;
      points.(2) <- p7;
      points.(3) <- p9;
      points.(4) <- p11;
      points.(5) <- p13; 
      points.(6) <- p15
    in 
    Hashtbl.add computed_points p points

  let () = 
    pre_computation get_g;
    List.iter ( fun p -> pre_computation p) points
  (** pre-compute points for the point multiplication, 
      implemented with the algorithm wNAF. 
      For window = 5 pre-compute the points {1,3,5,7,9,11,13,15} *)

  let double_and_add (p : point) (d : Z.t) =
    let d_bits = Z.format "%b" (neg_mod d get_n) in
    let q = ref Infinity in 
    String.iter (fun di -> 
        q := double_point !q ;
        if di =  '1' then
          q := add_point !q p 
        else ()) d_bits;
    !q

  let montogomery_ladders (p : point) (d : Z.t) =
    let d_bits = Z.format "%b" (neg_mod d get_n) in
    let r_0 = ref Infinity in
    let r_1 = ref p in
    String.iter (fun di ->
        if di = '0' then
          begin
            r_1 := add_point !r_0 !r_1 ;
            r_0 := double_point !r_0 
          end
        else
          begin
            r_0 := add_point !r_0 !r_1 ;
            r_1 := double_point !r_1 
          end) d_bits;
    (!r_0)

  let multiply_point (p : point) (d : Z.t) =
    let rec aux q pc_points = function
      | [] -> q 
      | ki :: t -> 
        let q2 = double_point q in 
        if ki <> Z.zero then
          let ki_p = 
            return_point p pc_points ki 
          in
          if ki > Z.zero then 
            aux (add_point q2 ki_p) pc_points t 
          else 
            aux (add_point q2 (inverse_point ki_p ) ) 
              pc_points t
        else aux q2 pc_points t 
    in
    let d = neg_mod d get_n in
    let w_naf_list = w_naf d 5 in  
    try 
      let pc_points = Hashtbl.find computed_points p in 
      aux Infinity pc_points w_naf_list
    with Not_found ->
      pre_computation p;
      let pc_points = Hashtbl.find computed_points p in
      aux Infinity pc_points w_naf_list

  let multiscalar_mul = 
    List.fold_left2 (fun acc s p -> 
        add_point (multiply_point p s ) acc )

end

