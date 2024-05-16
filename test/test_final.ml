open OUnit2
open Final.Cryptolib

(* SHA-256 hash tests *)
let test_sha256 _ =
  let input = "hello" in
  assert_equal (sha256 input)
    "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"

let test_sha256_empty _ =
  let input = "" in
  assert_equal (sha256 input)
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

let test_sha256_special_chars _ =
  let input = "!@#$%^&*()_+{}|:<>?" in
  assert_equal (sha256 input)
    "fcf4a2127882a013aa21259d43d924b79a3cdff35dce6b4e8030b9cddbc7d25a"

let test_sha256_long_input _ =
  let input = String.make 1000 'a' in
  assert_equal (sha256 input)
    "41edece42dcf625b6f5d17fa35a52c768a85038994748d360573cdcdcdb55d3c"

let test_sha256_unicode _ =
  let input = "こんにちは世界" in
  assert_equal (sha256 input)
    "10d2f8a16a5c3cc5d4eb1fc03e6e53e4c0ec66ab04a79ac61751f9acb2e7c2ed"

(* RIPEMD-160 hash tests *)
let test_ripemd160 _ =
  let input = "hello" in
  assert_equal (ripemd160 input) "108f07b8382412612c048d07d13f814118445acd"

let test_ripemd160_empty _ =
  let input = "" in
  assert_equal (ripemd160 input) "9c1185a5c5e9fc54612808977ee8f548b2258d31"

let test_ripemd160_special_chars _ =
  let input = "!@#$%^&*()_+{}|:<>?" in
  assert_equal (ripemd160 input) "87fc17019789c923bf6bda86d08d2859ad0bd9a0"

let test_ripemd160_long_input _ =
  let input = String.make 1000 'a' in
  assert_equal (ripemd160 input) "52783243c1697bdbe16d37f97f68f08325dc1528"

let test_ripemd160_unicode _ =
  let input = "こんにちは世界" in
  assert_equal (ripemd160 input) "09f1a2d44e57c8a1bc7fbcf1e1a1c2a5ab1c4fba"

(* Base58Check encoding tests *)
let test_base58check _ =
  let input = "108f07b8382412612c048d07d13f814118445acd" in
  assert_equal (base58check input) "16UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS"

let test_base58check_empty _ =
  let input = "" in
  assert_equal (base58check input) "3QJmnh"

let test_base58check_special_chars _ =
  let input = "0001020304050607" in
  assert_equal (base58check input) "14G5RWKh4x7uA9JHjpMTjKrFe1YvGsCwH5"

let test_base58check_leading_zeroes _ =
  let input =
    "0000000000000000000000000000000000000000000000000000000000000000"
  in
  assert_equal (base58check input) "11111111111111111111111111111111"

let test_base58check_long_input _ =
  let input = String.make 100 'a' in
  assert_equal (base58check input) "14Zn83NiTVKZ5u6TX7tEfWfAvpN3UzF41k"

let test_base58check_edge_case _ =
  let input =
    "0000000000000000000000000000000000000000000000000000000000000001"
  in
  assert_equal (base58check input) "11111111111111111111111111111113"

(* Run all tests *)
let suite =
  "Cryptographic Tests"
  >::: [
         "test_sha256" >:: test_sha256;
         "test_sha256_empty" >:: test_sha256_empty;
         "test_sha256_special_chars" >:: test_sha256_special_chars;
         "test_sha256_long_input" >:: test_sha256_long_input;
         "test_sha256_unicode" >:: test_sha256_unicode;
         "test_ripemd160" >:: test_ripemd160;
         "test_ripemd160_empty" >:: test_ripemd160_empty;
         "test_ripemd160_special_chars" >:: test_ripemd160_special_chars;
         "test_ripemd160_long_input" >:: test_ripemd160_long_input;
         "test_ripemd160_unicode" >:: test_ripemd160_unicode;
         "test_base58check" >:: test_base58check;
         "test_base58check_empty" >:: test_base58check_empty;
         "test_base58check_special_chars" >:: test_base58check_special_chars;
         "test_base58check_leading_zeroes" >:: test_base58check_leading_zeroes;
         "test_base58check_long_input" >:: test_base58check_long_input;
         "test_base58check_edge_case" >:: test_base58check_edge_case;
       ]

let () = run_test_tt_main suite
