open OUnit2
open Final.Cryptolib
open Final.Secp256k1

(** Private Key: 6e275a299ce5e712d129fdcfd62b55750cc18030400957db1b8a8c522
  16ef9c9 *)
let test_sha256 _ =
  let pubk =
    "03f26143ad902f37be8c1bef841685a1c3f6d463bb382daa97420617fd3ca17ec6"
  in
  assert_equal (sha256 pubk)
    "7aa0a581dae40ec7c81d1c40cdbfe7b8267728d56b5ee13e2833502bb2398262"

let test_ripemd160 _ =
  let hash =
    "7aa0a581dae40ec7c81d1c40cdbfe7b8267728d56b5ee13e2833502bb2398262"
  in
  assert_equal (ripemd160 hash) "b11dcf7ac755fbd940b5b76f2c01f59172dea5ce"

let test_base58check _ =
  let input = "b11dcf7ac755fbd940b5b76f2c01f59172dea5ce" in
  assert_equal (base58check input) "2t6x1P1CKKVn3bWxqgxNE9Miu1S8gLzH"

let test_generate_public_key _ =
  let input =
    "e3692380f2a38dcb1ff176ae5d2c6d3b0fb73041ed542cef88ab1e90dad72602"
  in
  assert_equal
    (generate_public_key input)
    "024e03f21eaffaeca72c70f0143e49fa2718e328de2c7ad44e0f2f0ac2488e2d9d"

let test_suite =
  "Tests"
  >::: [
         "compute a SHA-256 hash" >:: test_sha256;
         "compute a RIPEMD-160 hash" >:: test_ripemd160;
         "compute the result of a Base58Check" >:: test_base58check;
         "generate a correct public key" >:: test_generate_public_key;
       ]

let () = OUnit2.run_test_tt_main test_suite
