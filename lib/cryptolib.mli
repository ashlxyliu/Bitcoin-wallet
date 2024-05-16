val sha256 : string -> string
(** [sha256 str] returns the SHA-256 hash of the input string [str] encoded in 
    hexadecimal format. *)

val ripemd160 : string -> string
(** [ripemd160 input] returns the RIPEMD-160 hash of the input string [input] 
    encoded in hexadecimal format. *)

val list_of_string : string -> char list
(** [list_of_string str] converts the input string [str] to a list of 
    characters. *)

val base58alphabet : char list
(** The Base58 alphabet represented as a list of characters. *)

val hex_to_bigint : string -> Z.t
(** [hex_to_bigint hex_str] converts the input hexadecimal string [hex_str] to 
    a big integer. *)

val base58 : string -> string
(** [base58 input] encodes the input hexadecimal string [input] in Base58. *)

val base58check : string -> string
(** [base58check input] encodes the input hexadecimal string [input] using 
    Base58Check encoding. *)
