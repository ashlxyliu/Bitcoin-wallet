(* bin/scraper.ml *)

open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

(* Function to create the URI for the API request *)
let create_uri () =
  let base_uri = "https://api.coingecko.com/api/v3/simple/price" in
  let query = "?ids=bitcoin,ethereum&vs_currencies=usd" in
  Uri.of_string (base_uri ^ query)

(* Function to make the HTTP GET request *)
let make_request uri =
  Client.get uri >>= fun (resp, body) -> Lwt.return (resp, body)

(* Function to check if the response status is OK *)
let is_response_ok resp =
  let code = resp |> Response.status |> Code.code_of_status in
  code = 200

(* Function to convert response body to string *)
let body_to_string body = body |> Cohttp_lwt.Body.to_string

(* Function to parse JSON string to Yojson.Basic.json *)
let parse_json body_str = Yojson.Basic.from_string body_str

(* Helper function to extract float value from Yojson.Basic.json *)
let to_float_safe json =
  match Yojson.Basic.Util.to_number_option json with
  | Some num -> num
  | None ->
      raise
        (Yojson.Basic.Util.Type_error
           ("Expected float, got " ^ Yojson.Basic.pretty_to_string json, json))

(* Function to extract prices from JSON *)
let extract_prices json =
  let open Yojson.Basic.Util in
  let btc_price = json |> member "bitcoin" |> member "usd" |> to_float_safe in
  let eth_price = json |> member "ethereum" |> member "usd" |> to_float_safe in
  (btc_price, eth_price)

(* Function to display the prices *)
let display_prices (btc_price, eth_price) =
  Printf.printf "Bitcoin Price: $%.2f\n" btc_price;
  Printf.printf "Ethereum Price: $%.2f\n" eth_price

(* Main function to orchestrate fetching and displaying prices *)
let fetch_and_display_prices () =
  let uri = create_uri () in
  make_request uri >>= fun (resp, body) ->
  if is_response_ok resp then
    body_to_string body >>= fun body_str ->
    let json = parse_json body_str in
    let prices = extract_prices json in
    Lwt_io.printf "Prices fetched successfully!\n" >>= fun () ->
    Lwt_io.flush_all () >>= fun () -> Lwt.return (display_prices prices)
  else
    Lwt_io.printf "Failed to fetch price data\n" >>= fun () ->
    Lwt_io.flush_all ()

(* Entry point for the scraper module *)
let main () =
  let _ = fetch_and_display_prices () in
  Lwt_main.run (Lwt.return ())
