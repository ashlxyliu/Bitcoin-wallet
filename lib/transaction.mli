type input = {
  prev_txid : string;
  vout : int;
  script_sig : string;
  sequence : int64;
}

type output = { value : int64; script_pubkey : string }

type transaction = {
  version : int32;
  inputs : input list;
  outputs : output list;
  locktime : int32;
}

val create_input : string -> int -> string -> int64 -> input
(** [create_input prev_txid vout script_sig sequence] creates a new transaction 
    input. *)

val create_output : int64 -> string -> output
(** [create_output value script_pubkey] creates a new transaction output. *)

val create_transaction :
  int32 -> input list -> output list -> int32 -> transaction
(** [create_transaction version inputs outputs locktime] creates a new 
    transaction. *)

val serialize_input : input -> string
(** [serialize_input input] serializes a transaction input to a string. *)

val serialize_output : output -> string
(** [serialize_output output] serializes a transaction output to a string. *)

val serialize_transaction : transaction -> string
(** [serialize_transaction tx] serializes a transaction to a string. *)

val hash_transaction : transaction -> string
(** [hash_transaction tx] computes the hash of a transaction. *)

val sign_transaction : transaction -> string -> string
(** [sign_transaction tx privkey] signs a transaction using the provided 
    private key. *)

val transaction_to_hex : transaction -> string -> string
(** [transaction_to_hex tx signatures] converts a signed transaction to a 
    hexadecimal string. *)

val create_and_sign_transaction :
  string -> int -> string -> int64 -> int64 -> string -> string -> string
(** [create_and_sign_transaction prev_txid vout script_sig sequence value 
    script_pubkey privkey] creates and signs a new transaction. *)
