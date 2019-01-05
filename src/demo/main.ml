open Ecc__EccPrimitives
open Ecc__Ecdh
open Ecc__Ecdsa

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

let main = 
  let msg = "ECDSA demo!" in
  let signature = sign_message msg alice_sk in
    verify_signed_message msg signature alice_pk;

