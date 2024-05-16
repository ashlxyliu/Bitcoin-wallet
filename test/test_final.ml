open OUnit2
open Final.Cryptolib
open Final.Secp256k1

(** SHA-256 hash tests *)
let test_sha256 _ =
  let pubk =
    "03f26143ad902f37be8c1bef841685a1c3f6d463bb382daa97420617fd3ca17ec6"
  in
  assert_equal (sha256 pubk)
    "7aa0a581dae40ec7c81d1c40cdbfe7b8267728d56b5ee13e2833502bb2398262"

let test_sha256_empty _ =
  let input = "" in
  assert_equal (sha256 input)
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

let test_sha256_special_chars _ =
  let input = "!@#$%^&*()_+{}|:<>?" in
  assert_equal (sha256 input)
    "853b5d8675f1ea3b3c44953be9822030538392eecafed3d2c3a2d83f8b1cf5b2"

let test_sha256_long_input _ =
  let input = String.make 1000 'a' in
  assert_equal (sha256 input)
    "41edece42dcf625b6f5d17fa35a52c768a85038994748d360573cdcdcdb55d3c"

let test_sha256_unicode _ =
  let input = "こんにちは世界" in
  assert_equal (sha256 input)
    "ad78f29505c7bcd1e33050c92a4535306eb7f5e26e820b50b1d4b1633e2a5c67"

(** RIPEMD-160 hash tests *)
let test_ripemd160 _ =
  let hash =
    "7aa0a581dae40ec7c81d1c40cdbfe7b8267728d56b5ee13e2833502bb2398262"
  in
  assert_equal (ripemd160 hash) "b11dcf7ac755fbd940b5b76f2c01f59172dea5ce"

let test_ripemd160_empty _ =
  let input = "" in
  assert_equal (ripemd160 input) "9c1185a5c5e9fc54612808977ee8f548b2258d31"

let test_ripemd160_special_chars _ =
  let input = "!@#$%^&*()_+{}|:<>?" in
  assert_equal (ripemd160 input) "7da8489740d5fe33e53ff8d30b127eddb5127723"

let test_ripemd160_long_input _ =
  let input = String.make 1000 'a' in
  assert_equal (ripemd160 input) "3cdee36f05d7b7c1a9e324c2f85d6e4e8f2a0125"

let test_ripemd160_unicode _ =
  let input = "こんにちは世界" in
  assert_equal (ripemd160 input) "5c7059b971ad1c04d1ab08de2eec28726a2e0b7a"

(** Base58Check encoding tests *)
let test_base58check _ =
  let input = "b11dcf7ac755fbd940b5b76f2c01f59172dea5ce" in
  assert_equal (base58check input) "2t6x1P1CKKVn3bWxqgxNE9Miu1S8gLzH"

let test_base58check_empty _ =
  let input = "" in
  assert_equal (base58check input) "1Wh4bh"

let test_base58check_special_chars _ =
  let input = "00!@#$%^&*()_+{}|:<>?" in
  assert_equal (base58check input) "14Qu9MhZWMTVdLg1bPpRZx2mg7k"

let test_base58check_leading_zeroes _ =
  let input =
    "0000000000000000000000000000000000000000000000000000000000000000"
  in
  assert_equal (base58check input) "11111111111111111111111111111111"

let test_base58check_long_input _ =
  let input = String.make 100 'a' in
  assert_equal (base58check input) "1aWPMYhu7GN6VEfJUnMTvwVrPo"

(** Public key generation tests *)
let test_generate_public_key _ =
  let input =
    "e3692380f2a38dcb1ff176ae5d2c6d3b0fb73041ed542cef88ab1e90dad72602"
  in
  assert_equal
    (generate_public_key input)
    "024e03f21eaffaeca72c70f0143e49fa2718e328de2c7ad44e0f2f0ac2488e2d9d"

let test_generate_public_key_zero _ =
  let input =
    "0000000000000000000000000000000000000000000000000000000000000000"
  in
  assert_raises (Failure "Invalid private key") (fun () ->
      generate_public_key input)

let test_generate_public_key_short _ =
  let input = "1234" in
  assert_raises (Failure "Invalid private key") (fun () ->
      generate_public_key input)

let test_generate_public_key_long _ =
  let input =
    "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  in
  assert_raises (Failure "Invalid private key") (fun () ->
      generate_public_key input)

let test_generate_public_key_edge_case _ =
  let input =
    "0000000000000000000000000000000000000000000000000000000000000001"
  in
  assert_equal
    (generate_public_key input)
    "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

(** Comprehensive test suite *)
let test_suite =
  "Cryptography Tests"
  >::: [
         "compute a SHA-256 hash" >:: test_sha256;
         "compute a SHA-256 hash for empty string" >:: test_sha256_empty;
         "compute a SHA-256 hash for special characters"
         >:: test_sha256_special_chars;
         "compute a SHA-256 hash for long input" >:: test_sha256_long_input;
         "compute a SHA-256 hash for Unicode characters" >:: test_sha256_unicode;
         "compute a RIPEMD-160 hash" >:: test_ripemd160;
         "compute a RIPEMD-160 hash for empty string" >:: test_ripemd160_empty;
         "compute a RIPEMD-160 hash for special characters"
         >:: test_ripemd160_special_chars;
         "compute a RIPEMD-160 hash for long input"
         >:: test_ripemd160_long_input;
         "compute a RIPEMD-160 hash for Unicode characters"
         >:: test_ripemd160_unicode;
         "compute the result of a Base58Check" >:: test_base58check;
         "compute the result of a Base58Check for empty string"
         >:: test_base58check_empty;
         "compute the result of a Base58Check for special characters"
         >:: test_base58check_special_chars;
         "compute the result of a Base58Check for leading zeroes"
         >:: test_base58check_leading_zeroes;
         "compute the result of a Base58Check for long input"
         >:: test_base58check_long_input;
         "generate a correct public key" >:: test_generate_public_key;
         "generate a public key for zero private key"
         >:: test_generate_public_key_zero;
         "generate a public key for short private key"
         >:: test_generate_public_key_short;
         "generate a public key for long private key"
         >:: test_generate_public_key_long;
         "generate a public key for edge case private key"
         >:: test_generate_public_key_edge_case;
       ]

(* Run the test suite *)
let () = OUnit2.run_test_tt_main test_suite
