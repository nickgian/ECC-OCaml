open Ecc
include Ecc

let main =
  let curve = brainpool_P256_r1 in
    Printf.printf "Enter your name: ";
    flush stdout;
    let user = Scanf.scanf "%s\n" (fun s -> s) in
      match (try Some (open_out_gen [Open_wronly; Open_creat; Open_excl] 0o664 (String.concat "" ["users/"; user; ".pk"]))
             with Sys_error _ -> None) with
        | None -> 
            Printf.printf "User already exists.\nIf you are %s use your PK else retry with a different name\n" user
        | Some pk_out -> 
            let (pk_x, pk_y, sk) = create_keys curve in
              flush stdout;
              let sk_out = open_out (String.concat "" ["users/"; user; ".sk"]) in
                Printf.fprintf pk_out "%s\n" pk_x;
                Printf.fprintf pk_out "%s\n" pk_y;
                Printf.fprintf sk_out "%s\n" sk;
