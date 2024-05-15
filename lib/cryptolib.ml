open Cryptokit

let sha256 str =
  let hash = Hash.sha256 () in
  hash#add_string str;
  transform_string (Hexa.encode ()) hash#result

let ripemd160 input =
  let hash = Hash.ripemd160 () in
  hash#add_string input;
  transform_string (Hexa.encode ()) hash#result

let list_of_string str =
  let rec aux i acc = if i < 0 then acc else aux (i - 1) (str.[i] :: acc) in
  aux (String.length str - 1) []

let base58alphabet =
  let str = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  list_of_string str

let hex_to_bigint hex_str = Z.of_string_base 16 hex_str

let base58 input =
  let num_zeroes = ref 0 in
  let i = ref 0 in
  (* Handle leading zeroes *)
  while !i < String.length input && input.[!i] = '0' && input.[!i + 1] = '0' do
    num_zeroes := !num_zeroes + 1;
    i := !i + 2
  done;

  let num = hex_to_bigint (String.sub input !i (String.length input - !i)) in
  let alphabet = Array.of_list base58alphabet in
  let rec convert_to_base58 num acc =
    if Z.equal num Z.zero then acc
    else
      let remainder = Z.to_int (Z.rem num (Z.of_int 58)) in
      let next_num = Z.div num (Z.of_int 58) in
      convert_to_base58 next_num (alphabet.(remainder) :: acc)
  in
  let chars = convert_to_base58 num [] in
  String.concat "" (List.map (String.make 1) chars)

let base58check input =
  let str = "00" ^ input in
  let checksum = String.sub (sha256 (sha256 str)) 0 4 in
  let next_str = checksum ^ str in
  base58 next_str
