open EccPrimitives
open Ecdh
open Ecdsa

module DH = Ecdh (PrimeField)
module DSA = Ecdsa (PrimeField)

let curve = PrimeField.lookup_curve "brainpoolp256r1"


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
let main = main1 ;; main2
