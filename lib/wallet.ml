let generate_private_key () =
  let random_string = Cryptokit.Random.string Cryptokit.Random.secure_rng 32 in
  Cryptokit.transform_string (Cryptokit.Hexa.encode ()) random_string

let generate_public_key privk = Secp256k1.generate_public_key privk
