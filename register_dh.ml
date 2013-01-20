open Ecc
include Ecc

(*Generating random ints with a maximum length of decimal numbers*)
let random_big_int maxSize =
  Random.self_init ();
  let size = 1 + Random.int (maxSize - 1) in
  let big_int = String.create size in
  let rand_str = String.map (fun c -> 
                               let i = 48 + (Random.int 9) in Char.chr i) big_int in
    Z.of_string rand_str

(*Create public key*)

let rec create_keys curve = 
  let d_size = String.length (Z.to_string (curve.n)) in
  let sk = random_big_int d_size in
  let pk = multiply_point (curve.g) sk curve in
    match pk with
      | Infinity -> failwith "infinity\n"
      | Point (pk_x, pk_y) -> 
          begin
            match is_point (Point (pk_x, pk_y)) curve with
              | true -> 
                  Printf.printf "true\n";
                  (Z.to_string pk_x, Z.to_string pk_y, Z.to_string sk)
              | false -> 
                  failwith "false\n"
          end
let main =
  Printf.printf "Enter your name: ";
  flush stdout;
  let user = Scanf.scanf "%s\n" (fun s -> s) in
  let curve = brainpool_P256_r1 in

    match (try Some (open_out_gen [Open_wronly; Open_creat; Open_excl] 0o664 (String.concat "" ["users/"; user; ".pk"]))
           with Sys_error _ -> None) with
      | None -> 
          Printf.printf "User already exists.\nIf you are %s use your PK else retry with a different name\n" user
      | Some pk_out -> 
          let (pk_x, pk_y, sk) = create_keys curve in
            Printf.printf "kollisaaa\n";
            flush stdout;
          let sk_out = open_out (String.concat "" ["users/"; user; ".sk"]) in
            Printf.printf "kollisa\n";
            Printf.fprintf pk_out "%s\n" pk_x;
            Printf.fprintf pk_out "%s\n" pk_y;
            Printf.fprintf sk_out "%s\n" sk;
