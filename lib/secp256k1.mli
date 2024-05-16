(* Define the type for a point on the elliptic curve *)
type point = Infinity | Point of Z.t * Z.t

(* The prime number defining the field *)
val p : Z.t

(* The base point on the elliptic curve *)
val g : point

(* Function to add two points on the elliptic curve *)
val point_add : point -> point -> point

(* Function to double a point on the elliptic curve *)
val point_double : point -> point

(* Function to perform scalar multiplication on the elliptic curve *)
val scalar_mult : Z.t -> point -> point

(* Function to generate a public key from a private key *)
val generate_public_key : string -> string
