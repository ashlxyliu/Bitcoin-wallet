val create_price_uri : string -> Uri.t
(** [create_price_uri coin] returns the URI to fetch the price of the given coin. *)

val create_balance_uri : string -> Uri.t
(** [create_balance_uri address] returns the URI to fetch the balance of the given address. *)

val make_request : Uri.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
(** [make_request uri] sends a GET request to the given URI and returns the response and body. *)

val is_response_ok : Cohttp.Response.t -> bool
(** [is_response_ok resp] checks if the response status code is 200. *)

val body_to_string : Cohttp_lwt.Body.t -> string Lwt.t
(** [body_to_string body] converts the response body to a string. *)

val parse_json : string -> Yojson.Basic.t
(** [parse_json body_str] parses a JSON string into a Yojson.Basic.t type. *)

val to_float_safe : Yojson.Basic.t -> float
(** [to_float_safe json] safely extracts a float from a Yojson.Basic.t type. *)

val extract_price : string -> Yojson.Basic.t -> float
(** [extract_price coin json] extracts the price of the given coin from the JSON. *)

val extract_balance : Yojson.Basic.t -> float
(** [extract_balance json] extracts the balance from the JSON. *)

val display_price : string -> float -> unit
(** [display_price coin price] prints the price of the given coin. *)

val display_balance : float -> unit
(** [display_balance balance] prints the balance in BTC. *)

val fetch_and_display_price : string -> unit Lwt.t
(** [fetch_and_display_price coin] fetches and displays the price of the given coin. *)

val fetch_and_display_prices_loop : string list -> float -> unit Lwt.t
(** [fetch_and_display_prices_loop coins interval] periodically fetches and displays the prices of the given coins at the specified interval. *)

val fetch_and_display_balance : string -> unit Lwt.t
(** [fetch_and_display_balance address] fetches and displays the balance of the given address. *)
