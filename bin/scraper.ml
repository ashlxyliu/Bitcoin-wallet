open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

exception APIError of string

let create_price_uri () =
  let base_uri = "https://api.coingecko.com/api/v3/simple/price" in
  let query = "?ids=bitcoin,ethereum&vs_currencies=usd" in
  Uri.of_string (base_uri ^ query)

let create_balance_uri address =
  let base_uri = "https://api.blockcypher.com/v1/btc/main/addrs/" in
  Uri.of_string (base_uri ^ address ^ "/balance")

let make_request uri =
  Client.get uri >>= fun (resp, body) -> Lwt.return (resp, body)

let is_response_ok resp =
  let code = resp |> Response.status |> Code.code_of_status in
  code = 200

let body_to_string body = body |> Cohttp_lwt.Body.to_string

let parse_json body_str =
  try Yojson.Basic.from_string body_str
  with e -> raise (APIError ("JSON parsing failed: " ^ Printexc.to_string e))

let to_float_safe json =
  match Yojson.Basic.Util.to_number_option json with
  | Some num -> num
  | None ->
      raise
        (Yojson.Basic.Util.Type_error
           ("Expected float, got " ^ Yojson.Basic.pretty_to_string json, json))

let extract_prices json =
  let btc_price = json |> member "bitcoin" |> member "usd" |> to_float_safe in
  let eth_price = json |> member "ethereum" |> member "usd" |> to_float_safe in
  (btc_price, eth_price)

let extract_balance json = json |> member "balance" |> to_float_safe

let display_prices (btc_price, eth_price) =
  Printf.printf "Bitcoin Price: $%.2f\n" btc_price;
  Printf.printf "Ethereum Price: $%.2f\n" eth_price

let display_balance balance =
  Printf.printf "Wallet Balance: %.8f BTC\n" (balance /. 1_000_000_000.)

let fetch_and_display_prices () =
  let uri = create_price_uri () in
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

let fetch_and_display_balance address =
  let uri = create_balance_uri address in
  make_request uri >>= fun (resp, body) ->
  if is_response_ok resp then
    body_to_string body >>= fun body_str ->
    let json = parse_json body_str in
    let balance = extract_balance json in
    Lwt_io.printf "Balance fetched successfully!\n" >>= fun () ->
    Lwt_io.flush_all () >>= fun () -> Lwt.return (display_balance balance)
  else
    Lwt_io.printf "Failed to fetch balance data\n" >>= fun () ->
    Lwt_io.flush_all ()

(*
   open Lwt.Infix
   open Cohttp
   open Cohttp_lwt_unix
   open Yojson.Basic.Util

   exception APIError of string

   let create_price_uri coin =
     let base_uri = "https://api.coingecko.com/api/v3/simple/price" in
     let query = Printf.sprintf "?ids=%s&vs_currencies=usd" coin in
     Uri.of_string (base_uri ^ query)

   let create_balance_uri address =
     let base_uri = "https://api.blockcypher.com/v1/btc/main/addrs/" in
     Uri.of_string (base_uri ^ address ^ "/balance")

   let make_request uri =
     Client.get uri >>= fun (resp, body) -> Lwt.return (resp, body)

   let is_response_ok resp =
     let code = resp |> Response.status |> Code.code_of_status in
     code = 200

   let body_to_string body = body |> Cohttp_lwt.Body.to_string

   let parse_json body_str =
     try Yojson.Basic.from_string body_str
     with e -> raise (APIError ("JSON parsing failed: " ^ Printexc.to_string e))

   let to_float_safe json =
     match Yojson.Basic.Util.to_number_option json with
     | Some num -> num
     | None ->
         raise
           (Yojson.Basic.Util.Type_error
              ("Expected float, got " ^ Yojson.Basic.pretty_to_string json, json))

   let extract_price coin json =
     json |> member coin |> member "usd" |> to_float_safe

   let extract_balance json = json |> member "balance" |> to_float_safe

   let display_price coin price =
     Printf.printf "%s Price: $%.2f\n" (String.capitalize_ascii coin) price

   let display_balance balance =
     Printf.printf "Wallet Balance: %.8f BTC\n" (balance /. 1_000_000_000.)

   let fetch_and_display_price coin =
     let uri = create_price_uri coin in
     make_request uri >>= fun (resp, body) ->
     if is_response_ok resp then
       body_to_string body >>= fun body_str ->
       let json = parse_json body_str in
       let price = extract_price coin json in
       Lwt_io.printf "%s price fetched successfully!\n"
         (String.capitalize_ascii coin)
       >>= fun () ->
       Lwt_io.flush_all () >>= fun () -> Lwt.return (display_price coin price)
     else
       Lwt_io.printf "Failed to fetch %s price data\n"
         (String.capitalize_ascii coin)
       >>= fun () -> Lwt_io.flush_all ()

   let rec fetch_and_display_prices_loop coins interval =
     let rec fetch_prices coins =
       match coins with
       | [] -> Lwt.return_unit
       | coin :: rest ->
           fetch_and_display_price coin >>= fun () -> fetch_prices rest
     in
     Lwt_io.printf "Fetching prices...\n" >>= fun () ->
     fetch_prices coins >>= fun () ->
     Lwt_unix.sleep interval >>= fun () ->
     fetch_and_display_prices_loop coins interval

   let fetch_and_display_balance address =
     let uri = create_balance_uri address in
     make_request uri >>= fun (resp, body) ->
     if is_response_ok resp then
       body_to_string body >>= fun body_str ->
       let json = parse_json body_str in
       let balance = extract_balance json in
       Lwt_io.printf "Balance fetched successfully!\n" >>= fun () ->
       Lwt_io.flush_all () >>= fun () -> Lwt.return (display_balance balance)
     else
       Lwt_io.printf "Failed to fetch balance data\n" >>= fun () ->
       Lwt_io.flush_all () *)
