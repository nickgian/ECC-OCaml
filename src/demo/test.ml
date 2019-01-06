open EccPrimitives
open Ecdh
open Ecdsa

module DH = Ecdh (PrimeField)
module DSA = Ecdsa (PrimeField)

let curve = PrimeField.lookup_curve "secp256k1"


let (bob_pk, bob_sk) = DH.create_keys curve
let (alice_pk, alice_sk) = DH.create_keys curve

let sign_message msg sk =
  DSA.sign msg sk curve

let verify_signed_message msg sign pk_sender =
  if DSA.verify msg sign pk_sender curve then
    Printf.printf "message succesfully verified\n"
  else
    Printf.printf "Invalid signature\n"




let main1 = 
  let msg = "ECDSA demo!" in
  let signature = sign_message msg alice_sk in
    verify_signed_message msg signature alice_pk;
;;
let main2 =
  let bobs_secret = DH.create_session_key alice_pk bob_sk curve in
  let alices_secret = DH.create_session_key bob_pk alice_sk curve in
    (* In a world without war alice and bob should share a secret *)
    if (bobs_secret = alices_secret) then
      Printf.printf "Bob and alice shared a secret\n"
    else
      Printf.printf "We failed bob and alice\n"
;;


let ripemd160_hexdigest s =
let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode()) s in
  hex (Cryptokit.hash_string (Cryptokit.Hash.ripemd160()) s)
let sha256_hexdigest s =
  let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode()) s in
  hex (Cryptokit.hash_string (Cryptokit.Hash.sha256()) s)
    


let main3 =

  let sk = Z.of_string_base 16 "18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725" in
  let pk = DH.pubkey_of_seckey curve sk in
  let pkb = EccPrimitives.bytes_of_point_compressed pk in
  let pks = EccPrimitives.hexstring_of_string pkb in

  (* let pkhash = (Sha256.to_hex (Sha256.string pkb)) in *)

  let pkhashb = (Sha256.to_bin (Sha256.string pkb)) in

  let pkhash = sha256_hexdigest pkb in
  
  let rp = ripemd160_hexdigest pkhashb in
  Printf.printf "--------------------------TESTS---------------------\n";

  let rnd_str = "RANDOM STRING" in
  if EccPrimitives.hexstring_to_string
      (EccPrimitives.hexstring_of_string rnd_str) = rnd_str
  then Printf.printf "hexstring_to_string & hexstring_of_string OK\n"
  else Printf.printf "hexstring_to_string Fail %s\n" pks;

  if pks =
     "0250863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352"
  then Printf.printf "ECC OK\n"
  else Printf.printf "ECC Fail %s\n" pks;

  if pkhash =
     "0b7c28c9b7290c98d7438e70b3d3f7c848fbd7d1dc194ff83f4f7cc9b1378e98"
  then Printf.printf "SHA256 OK\n"
  else Printf.printf "SHA256 Fail %s\n" pkhash;

  if rp =
     "f54a5851e9372b87810a8e60cdd2e7cfd80b6e31"
  then Printf.printf "RIPEMD160 OK\n"
  else Printf.printf "RIPEMD160 Fail %s\n" rp;

  Printf.printf "----------------------END OF TESTS------------------\n";
;;


let main = main1 ;; main2 ;; main3
