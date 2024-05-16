<<<<<<< HEAD
(** @author Eliza Konvicka (ejk236)
    @author Ashley Liu (awl77)
    @author Andy Marous (acm337)*)

=======
>>>>>>> 5ce9874 (Implement transactions, creating and signing transactions. User can generate a signature using the private key.)
open ANSITerminal
open Final.Wallet
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Final.Transaction (* Ensure Transaction module is opened *)

(* Define a custom exception for API errors *)
exception APIError of string

(* Define the filename for storing wallet addresses *)
let wallets_file = "wallets.csv"
let transactions_file = "transactions.csv"

(* Function to save a wallet address to a file *)
let save_wallet_address name typ address pk sk =
  let wallets = Csv.load wallets_file in
  Csv.save wallets_file ([ name; typ; address; pk; sk ] :: wallets)

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
  print_string [ Foreground Cyan ] "5. Create and Sign Transaction\n";
  print_string [ Foreground Cyan ] "6. Remove Wallet\n";
  print_string [ Foreground Cyan ] "7. Exit\n";
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
      create_and_sign_transaction_menu ();
      wait_for_enter ();
      main_menu ()
  | "6" ->
      remove_wallet ();
      wait_for_enter ();
      main_menu ()
  | "7" ->
      print_string [ green ] "Exiting... Goodbye!\n";
      exit 0
  | _ ->
      print_string [ red ] "Invalid option, please try again.\n";
      main_menu ()

(* Function to create a new wallet *)
and create_new_wallet () =
  print_string [ yellow ]
    "Please enter 'b' for a bitcoin wallet, or 'e' for an Ethereum wallet.\n";
  let typ = read_line () in
  match typ with
  | "b" ->
      print_string [ yellow ] "Enter a name for your new wallet: \n";
      let name = read_line () in
      let sk = generate_private_key () in
      let pk = generate_public_key sk in
      let address = generate_wallet_address pk in
      print_string [ yellow ]
        ("Your new wallet, " ^ name ^ ", has address: " ^ address ^ "\n");
      save_wallet_address name "bitcoin" address pk sk
  | _ ->
      print_string [ red ] "Invalid wallet type. ";
      wait_for_enter ()

(* Function to display active wallets *)
and display_active_wallets () =
  let concat_string_list lst =
    let rec aux lst acc =
      match lst with [] -> acc | h :: t -> aux t (acc ^ " " ^ h)
    in
    aux lst ""
  in
  let rec print_wallet_list lst =
    match lst with
    | [] -> ()
    | h :: t ->
        print_string [ green ] (concat_string_list h ^ "\n");
        print_wallet_list t
  in
  let wallets = Csv.load wallets_file in
  if List.length wallets = 0 then
    print_string [ red ] "No active wallets found.\n"
  else print_string [ yellow ] "Your active wallets:\n";
  print_wallet_list wallets

(* Helper function to get a list of wallet addresses *)
and get_address_list () =
  let wallets = Csv.load wallets_file in
  List.map (fun lst -> List.hd (List.tl lst)) wallets

(* Function to check the balance of a wallet *)
and check_balance () =
  let wallet_list = get_address_list () in
  if wallet_list = [] then print_string [ red ] "No active wallets found.\n"
  else
    let addresses = Array.of_list wallet_list in
    print_string [ yellow ]
      "Enter the name of the wallet you'd like to check:\n";
    let name = read_line () in
    let rec name_index lst acc =
      match lst with
      | [] -> acc
      | h :: t -> if List.hd h = name then acc else name_index t (acc + 1)
    in
    let i = name_index (Csv.load wallets_file) 0 in
    if i = List.length wallet_list then print_string [ red ] "Invalid input.\n"
    else
      let address = addresses.(i) in
      print_endline address;
      Lwt_main.run (fetch_and_display_balance address)

(* Function to remove a wallet address *)
and remove_wallet () =
  let wallets = Csv.load wallets_file in
  if List.length wallets = 0 then
    print_string [ red ] "No active wallets found.\n"
  else print_string [ yellow ] "Select a wallet to remove:\n";
  let name = read_line () in
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | h :: t -> if List.hd h = name then aux t acc else aux t (h :: acc)
  in
  let new_wallets = List.rev (aux wallets []) in
  Csv.save wallets_file new_wallets;
  print_string [ green ] "Removal operation successfully executed.\n"

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

(* Function to create and sign a transaction *)
and create_and_sign_transaction_menu () =
  print_string [ yellow ] "Enter previous transaction ID: ";
  let prev_txid = read_line () in
  print_string [ yellow ] "Enter vout: ";
  let vout = read_int () in
  print_string [ yellow ] "Enter scriptSig: ";
  let script_sig = read_line () in
  print_string [ yellow ] "Enter sequence: ";
  let sequence = Int64.of_string (read_line ()) in
  print_string [ yellow ] "Enter value: ";
  let value = Int64.of_string (read_line ()) in
  print_string [ yellow ] "Enter scriptPubKey: ";
  let script_pubkey = read_line () in
  print_string [ yellow ] "Enter private key: ";
  let privkey = read_line () in
  let signed_tx =
    create_and_sign_transaction prev_txid vout script_sig sequence value
      script_pubkey privkey
  in
<<<<<<< HEAD
  let get_current_time () =
    let tm = Unix.localtime (Unix.time ()) in
    let date =
      String.concat "-"
        [
          string_of_int (tm.tm_year + 1900);
          string_of_int (tm.tm_mon + 1);
          string_of_int tm.tm_mday;
        ]
    in
    let time =
      String.concat ":"
        [
          string_of_int tm.tm_hour;
          string_of_int tm.tm_min;
          string_of_int tm.tm_sec;
        ]
    in
    date ^ " " ^ time
  in
  let transactions = Csv.load transactions_file in
  Csv.save transactions_file ([ get_current_time (); signed_tx ] :: transactions);
=======
>>>>>>> 5ce9874 (Implement transactions, creating and signing transactions. User can generate a signature using the private key.)
  print_string [ green ] ("Signed transaction: " ^ signed_tx ^ "\n")

(* Entry point of the program *)
let () = main_menu ()
