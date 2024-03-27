open Mirage_crypto_ec

let sha256_hex data = Digestif.SHA256.(to_hex (digest_string data))

let generate_private_key seed =
  let midpoint = String.length seed / 2 in
  let first_half = String.sub seed 0 midpoint in
  let second_half = String.sub seed midpoint (String.length seed - midpoint) in
  let first_hash = sha256_hex first_half in
  let second_hash = sha256_hex second_half in
  first_hash ^ second_hash

(*
   let generate_public_key_from_private private_key_hex =
     let curve = P256.Dh.gen_key () in
     let private_key = Cstruct.of_hex private_key_hex in
     (* This is a dummy operation for illustration. In real applications, you'd perform ECC multiplication here. *)
     let public_key, _ = curve in
     public_key

   let () =
     (* This would be the hexadecimal representation of your private key. *)
     let private_key_hex =
       "4f3c8e3b8b7979f7ed8c71f9b66461d4f5b43f1b4c8aaf8c797cd7e7a56e95d2"
     in
     let public_key = generate_public_key_from_private private_key_hex in
     Printf.printf "Public Key: %s\n" (Cstruct.to_hex public_key) *)
