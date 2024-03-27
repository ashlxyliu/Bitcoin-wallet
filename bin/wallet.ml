let sha256_hex data = Digestif.SHA256.(to_hex (digest_string data))

let generate_private_key seed =
  let midpoint = String.length seed / 2 in
  let first_half = String.sub seed 0 midpoint in
  let second_half = String.sub seed midpoint (String.length seed - midpoint) in
  let first_hash = sha256_hex first_half in
  let second_hash = sha256_hex second_half in
  first_hash ^ second_hash

open Z

let prime =
  Z.of_string "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
(* Placeholder prime for illustration *)

type point = Infinity | Point of Z.t * Z.t

let mod_add a b = Z.((a + b) mod prime)
let mod_sub a b = Z.((a - b) mod prime)
let mod_mul a b = Z.(a * b mod prime)
let mod_inv a = Z.invert a prime
let mod_div a b = mod_mul a (mod_inv b)

let point_add p1 p2 =
  match (p1, p2) with
  | Infinity, _ | _, Infinity -> Infinity (* Simplification *)
  | Point (x1, y1), Point (x2, y2) ->
      if Z.equal x1 x2 then Infinity (* Placeholder for doubling case *)
      else
        let slope = mod_div (mod_sub y2 y1) (mod_sub x2 x1) in
        let x3 = mod_sub (mod_mul slope slope) (mod_add x1 x2) in
        let y3 = mod_sub (mod_mul slope (mod_sub x1 x3)) y1 in
        Point (x3, y3)

let rec scalar_mult k point =
  match k with
  | k when Z.equal k Z.zero -> Infinity
  | k when Z.equal k Z.one -> point
  | k when Z.equal (Z.erem k (Z.of_int 2)) Z.zero ->
      scalar_mult (Z.div k (Z.of_int 2)) (point_add point point)
  | _ -> point_add point (scalar_mult (Z.sub k (Z.of_int 1)) point)

let hex_to_bigint hex = Z.of_string_base 16 hex

let derive_public_key_from_private private_key_hex =
  let private_key = hex_to_bigint private_key_hex in
  let public_key = scalar_mult private_key g in
  public_key

let () =
  let seed = "some_random_seed_string" in
  let private_key_hex = generate_private_key seed in
  let public_key = derive_public_key_from_private private_key_hex in
  match public_key with
  | Infinity -> Printf.printf "Public Key: Infinity\n"
  | Point (x, y) ->
      Printf.printf "Public Key: (%s, %s)\n" (Z.to_string x) (Z.to_string y)
