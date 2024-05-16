val sha256 : string -> string
(** Determines the SHA-256 hash of an inputted string. *)

val ripemd160 : string -> string
(** Determines the RIPEMD-160 hash of an inputted hash. *)

val base58check : string -> string
(** Encodes a string using Base58Check encoding. *)
