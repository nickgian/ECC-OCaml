open Ecc
include Ecc

let get_pk user =
  let file = String.concat "" ["users/"; user; ".pk"] in
    match (try Some (open_in file) with Sys_error _ -> None) with
      | None -> None
      | Some input ->
          let pk_x = Z.of_string (input_line input) in
          let pk_y = Z.of_string (input_line input) in
            Some (Point (pk_x, pk_y))

let get_sk user =
  let file = String.concat "" ["users/"; user; ".sk"] in
    match (try Some (open_in file) with Sys_error _ -> None) with
      | None -> None
      | Some input -> Some (Z.of_string (input_line input))

let rec read_file acc =
  match try Some (input_line stdin) with End_of_file -> None with 
    | Some line -> read_file (line :: acc)
    | None -> String.concat "" (List.rev acc)

let main =
  let curve = brainpool_P256_r1 in
  let user = Sys.argv.(1) in
    match (get_pk user) with
      | None -> failwith "User not found"
      | Some pk ->
          let in_file = open_in (String.concat "" ["messages/"; user; ".msg"]) in
          let msg = input_line in_file in
          let r_str = input_line in_file in
          let s_str = input_line in_file in
          let (r, s) = (Z.of_string r_str, Z.of_string s_str) in
            (match (Ecc.verify msg (r, s) pk curve) with
               | false -> Printf.printf "Invalid signature\n"
               | true -> Printf.printf "Signature validated\n"
            )
