(** @author Eliza Konvicka (ejk236)
    @author Ashley Liu (awl77)
    @author Andy Marous (acm337)*)

open ANSITerminal
open Final.Wallet
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Final.Transaction

exception APIError of string

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
  let address_list = get_address_list () in
  match List.length address_list with
  | 0 ->
      print_string [ red ]
        "Error: please create a wallet before attempting to create a \
         transaction.\n";
      wait_for_enter ()
  | _ -> (
      print_string [ yellow ]
        "Enter the name of the wallet you'd like to send from:";
      let name = read_line () in
      let transaction_list = Csv.load transactions_file in
      let addresses = Array.of_list address_list in
      let rec name_index lst acc =
        match lst with
        | [] -> acc
        | h :: t -> if List.hd h = name then acc else name_index t (acc + 1)
      in
      let i = name_index (Csv.load wallets_file) 0 in
      if i = List.length address_list then
        print_string [ red ] "Invalid name.\n"
      else
        let address = addresses.(i) in
        print_string [ yellow ]
          "Enter your total amount to send (including transaction fee): ";
        let txamt = read_float () in
        let wallet_balance =
          let rec aux lst acc =
            match lst with
            | [] -> acc
            | h :: t -> aux t (float_of_string (List.hd (List.tl h)) +. acc)
          in
          aux transaction_list 0.0
        in
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
        let rec first_column lst =
          match lst with
          | [] -> [] (* Base case: empty list returns an empty list *)
          | [] :: xs ->
              first_column xs (* Skip empty sub-lists to avoid errors *)
          | (h :: _) :: xs ->
              h
              :: first_column
                   xs (* Take the head of each non-empty sub-list and recurse *)
        in
        let wallet =
          let rec aux lst =
            match lst with
            | [] -> failwith "Impossible: wallet does exist"
            | h :: t -> if List.hd h = name then h else aux t
          in
          aux (Csv.load wallets_file)
        in
        match wallet_balance -. txamt with
        | a when a > 0.0 ->
            let signature =
              create_and_sign_transaction
                (String.concat "" (first_column transaction_list))
                (int_of_float txamt) "abc123" 1234567890123L 9876543210987L
                (List.hd (List.tl (List.tl wallet)))
                (List.hd (List.tl (List.tl (List.tl wallet))))
            in
            Csv.save transactions_file
              ([
                 get_current_time (); string_of_float txamt; address; signature;
               ]
              :: transaction_list);
            print_string [ green ]
              "Transaction has been submitted to network for verification.\n";
            print_string [ blue ] ("Transaction signature: " ^ signature)
        | _ ->
            print_string [ red ]
              "Error: your desired amount to send exceeds that wallet's \
               balance.";
            ())

(* Entry point of the program *)
let () = main_menu ()
