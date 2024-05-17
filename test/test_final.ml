(* cryptolib_test.ml *)

open OUnit2
open Final.Cryptolib

let test_sha256 _ =
  let input = "test" in
  let expected =
    "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
  in
  let result = sha256 input in
  assert_equal ~printer:(fun x -> x) expected result

let suite = "Cryptolib Tests" >::: [ "test_sha256" >:: test_sha256 ]
let () = run_test_tt_main suite
