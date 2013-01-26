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

let main =
  let curve = brainpool_P256_r1 in
    Printf.printf "Enter your name: ";
    flush stdout;
    let user = Scanf.scanf "%s\n" (fun s -> s) in
      match (get_sk user) with
        | None -> failwith "User not found, register first\n"
        | Some sk ->
            Printf.printf "Select user to exchange key: ";
            flush stdout;
            let peer = Scanf.scanf "%s\n" (fun s -> s) in
              match (get_pk peer) with 
                | None -> failwith "User not found, try again\n"
                | Some pk ->
                    match multiply_point pk sk curve with
                      | Infinity -> failwith "internal error"
                      | Point (shared_key, _) -> Printf.printf "Shared key:\n %s\n" (Z.to_string shared_key)
