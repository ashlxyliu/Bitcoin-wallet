(* Function to generate a private key *)
let generate_private_key () =
  (* Generate a 32-byte random string using a secure random number generator *)
  let random_string = Cryptokit.Random.string Cryptokit.Random.secure_rng 32 in

  (* Encode the random string in hexadecimal format *)
  let hex_encoded_string =
    Cryptokit.transform_string (Cryptokit.Hexa.encode ()) random_string
  in

  (* Return the hexadecimal encoded string as the private key *)
  hex_encoded_string

(* Function to generate a public key from a private key *)
let generate_public_key privk =
  (* Call the Secp256k1 module's generate_public_key function with the private key *)
  let public_key = Secp256k1.generate_public_key privk in

  (* Return the generated public key *)
  public_key
