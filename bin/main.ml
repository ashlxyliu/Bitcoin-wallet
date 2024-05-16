open ANSITerminal
(** @author Eliza Konvicka (ejk236)
    @author Ashley Liu (awl77)
    @author Andy Marous (acm337)*)

open Final.Wallet
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

(* Define a custom exception for API errors *)
exception APIError of string

(* Define the filename for storing wallet addresses *)
let wallets_file = "wallets.txt"

(* Function to save a wallet address to a file *)
let save_wallet_address address =
  let out_channel =
    open_out_gen [ Open_append; Open_creat ] 0o666 wallets_file
  in
  output_string out_channel (address ^ "\n");
  close_out out_channel

(* Function to clear the terminal screen *)
let clear_screen () =
  Printf.printf "\027[2J";
  (* Clear the screen *)
  Printf.printf "\027[H" (* Move the cursor to the home position *)

(* Function to wait for the user to press Enter *)
let wait_for_enter () =
  print_string [ Reset ] "\nPress Enter to return to the main menu...";
  ignore (read_line ())

(* Function to print the main menu *)
let print_main_menu () =
  clear_screen ();
  print_string [ Foreground Cyan ] "Welcome to our Bitcoin wallet\n";
  print_string [ Foreground Cyan ] "Please select an option:\n";
  print_string [ Foreground Cyan ] "1. My Active Wallets\n";
  print_string [ Foreground Cyan ] "2. Create New Wallet\n";
  print_string [ Foreground Cyan ] "3. Fetch Coin Prices\n";
  print_string [ Foreground Cyan ] "4. Check Balance\n";
  print_string [ Foreground Cyan ] "5. Remove Wallet\n";
  print_string [ Foreground Cyan ] "6. Exit\n";
  print_string [ Reset ] "> "

(* Recursive function to display and handle the main menu *)
let rec main_menu () =
  print_main_menu ();
  match read_line () with
  | "1" ->
      display_active_wallets ();
      wait_for_enter ();
      main_menu ()
  | "2" ->
      create_new_wallet ();
      wait_for_enter ();
      main_menu ()
  | "3" ->
      Lwt_main.run (fetch_and_display_prices ());
      wait_for_enter ();
      main_menu ()
  | "4" ->
      check_balance ();
      wait_for_enter ();
      main_menu ()
  | "5" ->
      remove_wallet ();
      wait_for_enter ();
      main_menu ()
  | "6" ->
      print_string [ green ] "Exiting... Goodbye!\n";
      exit 0
  | _ ->
      print_string [ red ] "Invalid option, please try again.\n";
      main_menu ()

(* Function to create a new wallet *)
and create_new_wallet () =
  print_string [ yellow ] "Creating a new wallet...\n";
  let sk = generate_private_key () in
  let pk = generate_public_key sk in
  print_string [ yellow ] ("Your new wallet address is: " ^ pk ^ "\n");
  save_wallet_address pk

(* Function to display active wallets *)
and display_active_wallets () =
  print_string [ yellow ] "Your active wallets:\n";
  try
    let in_channel = open_in wallets_file in
    let rec read_wallets idx =
      try
        let address = input_line in_channel in
        print_string [ green ] (Printf.sprintf "%d. %s\n" idx address);
        read_wallets (idx + 1)
      with End_of_file -> close_in in_channel
    in
    read_wallets 1
  with Sys_error _ -> print_string [ red ] "No active wallets found.\n"

(* Helper function to get a list of wallet addresses *)
and get_wallet_list () =
  try
    let in_channel = open_in wallets_file in
    let rec read_wallets acc =
      try
        let address = input_line in_channel in
        read_wallets (address :: acc)
      with End_of_file ->
        close_in in_channel;
        List.rev acc
    in
    read_wallets []
  with Sys_error _ -> []

(* Function to check the balance of a wallet *)
and check_balance () =
  let wallet_list = get_wallet_list () in
  if wallet_list = [] then print_string [ red ] "No active wallets found.\n"
  else (
    print_string [ yellow ] "Select a wallet to check balance:\n";
    List.iteri
      (fun idx address ->
        print_string [ green ] (Printf.sprintf "%d. %s\n" (idx + 1) address))
      wallet_list;
    print_string [ Reset ] "> ";
    try
      let choice = read_int () in
      if choice < 1 || choice > List.length wallet_list then
        print_string [ red ] "Invalid choice.\n"
      else
        let address = List.nth wallet_list (choice - 1) in
        Lwt_main.run (fetch_and_display_balance address)
    with Failure _ -> print_string [ red ] "Invalid input.\n")

(* Function to remove a wallet address *)
and remove_wallet () =
  let wallet_list = get_wallet_list () in
  if wallet_list = [] then print_string [ red ] "No active wallets found.\n"
  else (
    print_string [ yellow ] "Select a wallet to remove:\n";
    List.iteri
      (fun idx address ->
        print_string [ green ] (Printf.sprintf "%d. %s\n" (idx + 1) address))
      wallet_list;
    print_string [ Reset ] "> ";
    try
      let choice = read_int () in
      if choice < 1 || choice > List.length wallet_list then
        print_string [ red ] "Invalid choice.\n"
      else
        let address_to_remove = List.nth wallet_list (choice - 1) in
        let temp_file = "temp_wallets.txt" in
        try
          let in_channel = open_in wallets_file in
          let out_channel = open_out temp_file in
          try
            while true do
              let address = input_line in_channel in
              if address <> address_to_remove then
                output_string out_channel (address ^ "\n")
            done;
            close_in in_channel;
            close_out out_channel;
            Sys.rename temp_file wallets_file;
            print_string [ green ] "Wallet address removed successfully.\n"
          with End_of_file ->
            close_in in_channel;
            close_out out_channel;
            Sys.rename temp_file wallets_file;
            print_string [ green ] "Wallet address removed successfully.\n"
        with Sys_error _ ->
          print_string [ red ] "Error removing wallet address.\n"
    with Failure _ -> print_string [ red ] "Invalid input.\n")

(* Function to fetch and display the prices of multiple cryptocurrencies *)
and fetch_and_display_prices () =
  let coins = [ "bitcoin"; "ethereum" ] in
  let rec fetch_prices coins =
    match coins with
    | [] -> Lwt.return_unit
    | coin :: rest ->
        fetch_and_display_price coin >>= fun () -> fetch_prices rest
  in
  fetch_prices coins

(* Function to fetch and display the price of a single cryptocurrency *)
and fetch_and_display_price coin =
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

(* Function to create the URI for fetching cryptocurrency prices *)
and create_price_uri coin =
  let base_uri = "https://api.coingecko.com/api/v3/simple/price" in
  let query = Printf.sprintf "?ids=%s&vs_currencies=usd" coin in
  Uri.of_string (base_uri ^ query)

(* Function to make an HTTP GET request *)
and make_request uri =
  Client.get uri >>= fun (resp, body) -> Lwt.return (resp, body)

(* Function to check if the HTTP response status is OK (200) *)
and is_response_ok resp =
  let code = resp |> Response.status |> Code.code_of_status in
  code = 200

(* Function to convert the response body to a string *)
and body_to_string body = Cohttp_lwt.Body.to_string body

(* Function to parse a JSON string *)
and parse_json body_str =
  try Yojson.Basic.from_string body_str
  with e -> raise (APIError ("JSON parsing failed: " ^ Printexc.to_string e))

(* Function to safely extract a float from a JSON value *)
and to_float_safe json =
  match Yojson.Basic.Util.to_number_option json with
  | Some num -> num
  | None ->
      raise
        (Yojson.Basic.Util.Type_error
           ("Expected float, got " ^ Yojson.Basic.pretty_to_string json, json))

(* Function to extract the price of a coin from a JSON object *)
and extract_price coin json =
  json |> member coin |> member "usd" |> to_float_safe

(* Function to display the price of a coin *)
and display_price coin price =
  Printf.printf "%s Price: $%.2f\n" (String.capitalize_ascii coin) price

(* Function to fetch and display the balance of a wallet address *)
and fetch_and_display_balance address =
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

(* Function to create the URI for fetching the balance of a wallet address *)
and create_balance_uri address =
  let base_uri = "https://api.blockcypher.com/v1/btc/main/addrs/" in
  Uri.of_string (base_uri ^ address ^ "/balance")

(* Function to extract the balance from a JSON object *)
and extract_balance json = json |> member "balance" |> to_float_safe

(* Function to display the balance of a wallet *)
and display_balance balance =
  Printf.printf "Wallet Balance: %.8f BTC\n" (balance /. 1_000_000_000.)

(* Entry point of the program *)
let () = main_menu ()
