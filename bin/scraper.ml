open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

(* Exception to be raised in case of API errors *)
exception APIError of string

(* Function to create the URI for fetching cryptocurrency prices *)
let create_price_uri coin =
  (* Base URI for the CoinGecko API *)
  let base_uri = "https://api.coingecko.com/api/v3/simple/price" in
  (* Query string to specify the coin and currency *)
  let query = Printf.sprintf "?ids=%s&vs_currencies=usd" coin in
  (* Combine the base URI and query string to form the full URI *)
  Uri.of_string (base_uri ^ query)

(* Function to create the URI for fetching the balance of a wallet address *)
let create_balance_uri address =
  (* Base URI for the BlockCypher API *)
  let base_uri = "https://api.blockcypher.com/v1/btc/main/addrs/" in
  (* Combine the base URI and address to form the full URI *)
  Uri.of_string (base_uri ^ address ^ "/balance")

(* Function to make an HTTP GET request to the given URI *)
let make_request uri =
  Client.get uri >>= fun (resp, body) -> Lwt.return (resp, body)

(* Function to check if the HTTP response status is OK (200) *)
let is_response_ok resp =
  let code = resp |> Response.status |> Code.code_of_status in
  code = 200

(* Function to convert the response body to a string *)
let body_to_string body = body |> Cohttp_lwt.Body.to_string

(* Function to parse a JSON string *)
let parse_json body_str =
  try Yojson.Basic.from_string body_str
  with e -> raise (APIError ("JSON parsing failed: " ^ Printexc.to_string e))

(* Function to safely extract a float from a JSON value *)
let to_float_safe json =
  match Yojson.Basic.Util.to_number_option json with
  | Some num -> num
  | None ->
      raise
        (Yojson.Basic.Util.Type_error
           ("Expected float, got " ^ Yojson.Basic.pretty_to_string json, json))

(* Function to extract the price of a coin from a JSON object *)
let extract_price coin json =
  json |> member coin |> member "usd" |> to_float_safe

(* Function to extract the balance from a JSON object *)
let extract_balance json = json |> member "balance" |> to_float_safe

(* Function to display the price of a coin *)
let display_price coin price =
  let coin_name = String.capitalize_ascii coin in
  Printf.printf "%s Price: $%.2f\n" coin_name price

(* Function to display the balance of a wallet *)
let display_balance balance =
  let balance_btc = balance /. 1_000_000_000. in
  Printf.printf "Wallet Balance: %.8f BTC\n" balance_btc

(* Function to fetch and display the price of a coin *)
let fetch_and_display_price coin =
  (* Create the URI for the coin price *)
  let uri = create_price_uri coin in
  (* Make the HTTP request *)
  make_request uri >>= fun (resp, body) ->
  if is_response_ok resp then
    (* Convert the response body to a string *)
    body_to_string body >>= fun body_str ->
    (* Parse the JSON from the response body *)
    let json = parse_json body_str in
    (* Extract the price from the JSON object *)
    let price = extract_price coin json in
    let coin_name = String.capitalize_ascii coin in
    (* Print a success message *)
    Lwt_io.printf "%s price fetched successfully!\n" coin_name >>= fun () ->
    Lwt_io.flush_all () >>= fun () ->
    (* Display the price *)
    Lwt.return (display_price coin price)
  else
    let coin_name = String.capitalize_ascii coin in
    (* Print a failure message *)
    Lwt_io.printf "Failed to fetch %s price data\n" coin_name >>= fun () ->
    Lwt_io.flush_all ()

(* Recursive function to fetch and display prices for a list of coins at regular intervals *)
let rec fetch_and_display_prices_loop coins interval =
  (* Helper function to fetch prices for a list of coins *)
  let rec fetch_prices coins =
    match coins with
    | [] -> Lwt.return_unit
    | coin :: rest ->
        fetch_and_display_price coin >>= fun () -> fetch_prices rest
  in
  (* Print a message indicating that prices are being fetched *)
  Lwt_io.printf "Fetching prices...\n" >>= fun () ->
  (* Fetch prices for all the coins in the list *)
  fetch_prices coins >>= fun () ->
  (* Wait for the specified interval *)
  Lwt_unix.sleep interval >>= fun () ->
  (* Repeat the process *)
  fetch_and_display_prices_loop coins interval

(* Function to fetch and display the balance of a wallet address *)
let fetch_and_display_balance address =
  (* Create the URI for the wallet balance *)
  let uri = create_balance_uri address in
  (* Make the HTTP request *)
  make_request uri >>= fun (resp, body) ->
  if is_response_ok resp then
    (* Convert the response body to a string *)
    body_to_string body >>= fun body_str ->
    (* Parse the JSON from the response body *)
    let json = parse_json body_str in
    (* Extract the balance from the JSON object *)
    let balance = extract_balance json in
    (* Print a success message *)
    Lwt_io.printf "Balance fetched successfully!\n" >>= fun () ->
    Lwt_io.flush_all () >>= fun () ->
    (* Display the balance *)
    Lwt.return (display_balance balance)
  else
    (* Print a failure message *)
    Lwt_io.printf "Failed to fetch balance data\n" >>= fun () ->
    Lwt_io.flush_all ()
