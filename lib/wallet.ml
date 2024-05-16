open Cryptolib

(* Function to generate a private key *)
let generate_private_key () =
  let random_string = Cryptokit.Random.string Cryptokit.Random.secure_rng 32 in
  let hex_encoded_string =
    Cryptokit.transform_string (Cryptokit.Hexa.encode ()) random_string
  in
  hex_encoded_string

(* Function to generate a public key from a private key *)
let generate_public_key privk =
  let public_key = Secp256k1.generate_public_key privk in
  public_key

(* Function to generate a wallet address from a public key *)
let generate_wallet_address pk =
  let sha = sha256 pk in
  let ripemd = ripemd160 sha in
  base58check ripemd
