(* Function to generate a private key *)
val generate_private_key : unit -> string

(* Function to generate a public key from a private key *)
val generate_public_key : string -> string
val generate_wallet_address : string -> string
