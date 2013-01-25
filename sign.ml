open Ecc
include Ecc

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
    match (get_sk user) with
      | None -> failwith "User not found, register first\n"
      | Some sk ->
          let msg = read_file [] in
          let (r, s) = Ecc.sign msg sk curve in
          let out = open_out (String.concat "" ["messages/"; user; ".msg"]) in
            Printf.fprintf out "%s\n%s\n%s\n" msg (Z.to_string r) (Z.to_string s)
