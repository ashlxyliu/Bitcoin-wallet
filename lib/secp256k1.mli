type point =
  | Infinity
  | Point of Z.t * Z.t
      (** The type [point] represents a point on the elliptic curve. It can be either 
    [Infinity] or a [Point] with coordinates of type [Z.t]. *)

val p : Z.t
(** [p] is the prime number defining the finite field over which the elliptic 
    curve is defined. *)

val g : point
(** [g] is the base point on the elliptic curve used for cryptographic 
    operations. *)

val point_add : point -> point -> point
(** [point_add p1 p2] returns the sum of the points [p1] and [p2] on the 
    elliptic curve. *)

val point_double : point -> point
(** [point_double p] returns the result of doubling the point [p] on the 
    elliptic curve. *)

val scalar_mult : Z.t -> point -> point
(** [scalar_mult k p] returns the result of multiplying the point [p] by the 
    scalar [k] on the elliptic curve. *)

val generate_public_key : string -> string
(** [generate_public_key privkey] generates a public key from the given private 
    key [privkey] and returns it as a hexadecimal string. *)

val sign : string -> string -> Z.t * Z.t
(** [sign privkey message] signs the [message] using the given private key 
    [privkey] and returns the signature as a pair of [Z.t] values (r, s). *)

val sign_to_hex : Z.t * Z.t -> string
(** [sign_to_hex (r, s)] converts the signature (r, s) to a hexadecimal 
    string. *)
