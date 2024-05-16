open Cryptolib
open Cryptokit

type input = {
  prev_txid : string;
  vout : int;
  script_sig : string;
  sequence : int64;
}
(** Type for an input in a transaction *)

type output = { value : int64; script_pubkey : string }
(** Type for an output in a transaction *)

type transaction = {
  version : int32;
  inputs : input list;
  outputs : output list;
  locktime : int32;
}
(** Type for a transaction *)

(** Function to create a new input *)
let create_input prev_txid vout script_sig sequence =
  { prev_txid; vout; script_sig; sequence }

(** Function to create a new output *)
let create_output value script_pubkey = { value; script_pubkey }

(** Function to create a new transaction *)
let create_transaction version inputs outputs locktime =
  { version; inputs; outputs; locktime }

(** Function to serialize an input *)
let serialize_input input =
  input.prev_txid
  ^ Printf.sprintf "%08x" input.vout
  ^ input.script_sig
  ^ Printf.sprintf "%016Lx" input.sequence

(** Function to serialize an output *)
let serialize_output output =
  Printf.sprintf "%016Lx" output.value ^ output.script_pubkey

(** Function to serialize a transaction *)
let serialize_transaction tx =
  let inputs_str = String.concat "" (List.map serialize_input tx.inputs) in
  let outputs_str = String.concat "" (List.map serialize_output tx.outputs) in
  Printf.sprintf "%08lx%s%s%08lx" tx.version inputs_str outputs_str tx.locktime

(** Function to hash a transaction *)
let hash_transaction tx = sha256 (serialize_transaction tx)

(** Function to sign a transaction *)
let sign_transaction tx privkey =
  let hash = hash_transaction tx in
  let r, s = Secp256k1.sign privkey hash in
  Secp256k1.sign_to_hex (r, s)

(** Function to convert a transaction and its signatures to a hexadecimal 
    string *)
let transaction_to_hex tx signatures =
  let serialized_tx = serialize_transaction tx in
  let signed_tx = serialized_tx ^ signatures in
  transform_string (Hexa.encode ()) signed_tx

(** Function to create and sign a new transaction *)
let create_and_sign_transaction prev_txid vout script_sig sequence value
    script_pubkey privkey =
  let input = create_input prev_txid vout script_sig sequence in
  let output = create_output value script_pubkey in
  let tx = create_transaction 1l [ input ] [ output ] 0l in
  let signed_tx = sign_transaction tx privkey in
  transaction_to_hex tx signed_tx
