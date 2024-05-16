open Cryptokit
open Z
(* open Cryptolib *)

type point = Infinity | Point of Z.t * Z.t

let string_p =
  "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"

let p = Z.of_string_base 16 string_p

let g =
  Point
    ( Z.of_string_base 16
        "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
      Z.of_string_base 16
        "483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8" )

let n =
  Z.of_string_base 16
    "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"

let point_add p1 p2 =
  match (p1, p2) with
  | Infinity, _ -> p2
  | _, Infinity -> p1
  | Point (x1, y1), Point (x2, y2) when Z.equal x1 x2 && Z.equal y1 (Z.neg y2)
    ->
      Infinity
  | Point (x1, y1), Point (x2, y2) ->
      let m = Z.((y2 - y1) * invert (x2 - x1) p) mod p in
      let x3 = Z.(powm m (Z.of_int 2) p - x1 - x2) mod p in
      let y3 = Z.((m * (x1 - x3)) - y1) mod p in
      Point (x3, y3)

let point_double p =
  match p with
  | Infinity -> Infinity
  | Point (x, y) ->
      let m = Z.of_int 3 * (x ** 2) / (Z.of_int 2 * y) in
      let newx = (m ** 2) - (Z.of_int 2 * x) in
      let newy = (m * (x - newx)) - y in
      Point (newx, newy)

let rec scalar_mult (k : Z.t) (p : point) =
  try
    match (Z.to_int k, p) with
    | _, Infinity -> Infinity (* k * O = O *)
    | 0, _ -> Infinity (* 0 * p = O *)
    | 1, _ -> p (* 1 * p = p *)
    | _, _ ->
        let q = if k mod Z.of_int 2 = Z.of_int 0 then p else Infinity in
        let p_doubled = point_double p in
        (* Double the point p *)
        let k_half = k / Z.of_int 2 in
        point_add q (scalar_mult k_half p_doubled)
  with Overflow ->
    if p = Infinity then Infinity
    else
      let q = if k mod Z.of_int 2 = Z.of_int 0 then p else Infinity in
      let p_doubled = point_double p in
      (* Double the point p *)
      let k_half = k / Z.of_int 2 in
      point_add q (scalar_mult k_half p_doubled)

let generate_public_key privk =
  let private_key = Z.of_string_base 16 privk in
  let point = scalar_mult private_key g in
  match point with
  | Infinity -> failwith "Invalid private key"
  | Point (x, y) ->
      let x_hex = Z.format "%x" x in
      (* Convert x to a hexadecimal string *)
      if Z.equal (Z.rem y (Z.of_int 2)) Z.zero then "02" ^ x_hex
      else "03" ^ x_hex

let sha256 str =
  let hash = Hash.sha256 () in
  hash#add_string str;
  hash#result |> transform_string (Hexa.encode ())

let sign privkey message =
  let z_privkey = Z.of_string_base 16 privkey in
  let z_message = Z.of_string_base 16 (sha256 message) in
  (* Generate a random nonce k. This should be securely generated in practice. *)
  let k =
    Z.of_string_base 16
      "3b9aca07d24c61b3d8b8d3ffbe59c9c6d84a26ec09dc59452f265b76a02e5c5e"
  in
  let r_point = scalar_mult k g in
  match r_point with
  | Infinity -> failwith "Generated point at infinity"
  | Point (rx, _) ->
      let r = Z.rem rx n in
      let s =
        Z.rem (Z.mul (Z.add z_message (Z.mul r z_privkey)) (Z.invert k n)) n
      in
      (r, s)

let sign_to_hex (r, s) =
  let r_hex = Z.format "%064x" r in
  let s_hex = Z.format "%064x" s in
  r_hex ^ s_hex
