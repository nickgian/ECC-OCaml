open EccPrimitives
open Ecdh
open Ecdsa

module DH = Ecdh (PrimeField)
module DSA = Ecdsa (PrimeField)

let curve = PrimeField.lookup_curve "brainpoolp256r1"

let (bob_pk, bob_sk) = DH.create_keys curve
let (alice_pk, alice_sk) = DH.create_keys curve

let main = 
  let bobs_secret = DH.create_session_key alice_pk bob_sk curve in
  let alices_secret = DH.create_session_key bob_pk alice_sk curve in
    (* In a world without war alice and bob should share a secret *)
    if (bobs_secret = alices_secret) then
      Printf.printf "Bob and alice shared a secret\n"
    else
      Printf.printf "We failed bob and alice\n"
