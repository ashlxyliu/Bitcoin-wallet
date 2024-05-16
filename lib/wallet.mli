val generate_private_key : unit -> string
(** [generate_private_key ()] generates a new private key and returns it as 
    a hexadecimal string. *)

val generate_public_key : string -> string
(** [generate_public_key privkey] generates a public key from the given private 
    key and returns it as a hexadecimal string. *)

val generate_wallet_address : string -> string
(** [generate_wallet_address pubkey] generates a wallet address from the given 
    public key using hashing and encoding steps, and returns it as a 
    Base58Check encoded string. *)
