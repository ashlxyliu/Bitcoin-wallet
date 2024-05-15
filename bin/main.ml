open ANSITerminal
open Final.Wallet
open Scraper

let wallets_file = "wallets.txt"

let save_wallet_address address =
  let out_channel =
    open_out_gen [ Open_append; Open_creat ] 0o666 wallets_file
  in
  output_string out_channel (address ^ "\n");
  close_out out_channel

let print_main_menu () =
  print_string [ Foreground Cyan ] "Welcome to our Bitcoin wallet\n";
  print_string [ Foreground Cyan ] "Please select an option:\n";
  print_string [ Foreground Cyan ] "1. My Active Wallets\n";
  print_string [ Foreground Cyan ] "2. Create New Wallet\n";
  print_string [ Foreground Cyan ] "3. Fetch Coin Prices\n";
  print_string [ Foreground Cyan ] "4. Check Balance\n";
  print_string [ Foreground Cyan ] "5. Remove Wallet\n";
  print_string [ Foreground Cyan ] "6. Exit\n";
  print_string [ Reset ] "> "

let rec main_menu () =
  print_main_menu ();
  match read_line () with
  | "1" ->
      display_active_wallets ();
      main_menu ()
  | "2" ->
      create_new_wallet ();
      main_menu ()
  | "3" ->
      Lwt_main.run (fetch_and_display_prices ());
      main_menu ()
  | "4" ->
      check_balance ();
      main_menu ()
  | "5" ->
      remove_wallet ();
      main_menu ()
  | "6" ->
      print_string [ green ] "Exiting... Goodbye!\n";
      exit 0
  | _ ->
      print_string [ red ] "Invalid option, please try again.\n";
      main_menu ()

and create_new_wallet () =
  print_string [ yellow ] "Creating a new wallet...\n";
  let sk = generate_private_key () in
  let pk = generate_public_key sk in
  print_string [ yellow ] ("Your new wallet address is: " ^ pk ^ "\n");
  save_wallet_address pk

and display_active_wallets () =
  print_string [ yellow ] "Your active wallets:\n";
  try
    let in_channel = open_in wallets_file in
    try
      while true do
        let address = input_line in_channel in
        print_string [ green ] (address ^ "\n")
      done
    with End_of_file -> close_in in_channel
  with Sys_error _ -> print_string [ red ] "No active wallets found.\n"

and check_balance () =
  print_string [ yellow ] "Enter wallet address to check balance: ";
  let address = read_line () in
  Lwt_main.run (fetch_and_display_balance address)

and remove_wallet () =
  print_string [ yellow ] "Enter wallet address to remove: ";
  let address_to_remove = read_line () in
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
  with Sys_error _ -> print_string [ red ] "Error removing wallet address.\n"

let () = main_menu ()

(* open ANSITerminal
   open Final.Wallet
   open Scraper
   (* open Lwt.Infix *)

   let wallets_file = "wallets.txt"

   let save_wallet_address address =
     let out_channel =
       open_out_gen [ Open_append; Open_creat ] 0o666 wallets_file
     in
     output_string out_channel (address ^ "\n");
     close_out out_channel

   let print_main_menu () =
     print_string [ Foreground Cyan ] "Welcome to our Bitcoin wallet\n";
     print_string [ Foreground Cyan ] "Please select an option:\n";
     print_string [ Foreground Cyan ] "1. My Active Wallets\n";
     print_string [ Foreground Cyan ] "2. Create New Wallet\n";
     print_string [ Foreground Cyan ] "3. Fetch Coin Prices\n";
     print_string [ Foreground Cyan ] "4. Check Balance\n";
     print_string [ Foreground Cyan ] "5. Remove Wallet\n";
     print_string [ Foreground Cyan ] "6. Exit\n";
     print_string [ Reset ] "> "

   let rec main_menu () =
     print_main_menu ();
     match read_line () with
     | "1" ->
         display_active_wallets ();
         main_menu ()
     | "2" ->
         create_new_wallet ();
         main_menu ()
     | "3" ->
         fetch_coin_prices ();
         main_menu ()
     | "4" ->
         check_balance ();
         main_menu ()
     | "5" ->
         remove_wallet ();
         main_menu ()
     | "6" ->
         print_string [ green ] "Exiting... Goodbye!\n";
         exit 0
     | _ ->
         print_string [ red ] "Invalid option, please try again.\n";
         main_menu ()

   and fetch_coin_prices () =
     print_string [ yellow ]
       "Enter the coin to fetch the price (e.g., bitcoin, ethereum): ";
     let coin = read_line () in
     Lwt_main.run (fetch_and_display_price coin);
     print_string [ yellow ] "Press Enter to return to the main menu...";
     ignore (read_line ())

   and create_new_wallet () =
     print_string [ yellow ] "Creating a new wallet...\n";
     let sk = generate_private_key () in
     let pk = generate_public_key sk in
     print_string [ yellow ] ("Your new wallet address is: " ^ pk ^ "\n");
     save_wallet_address pk;
     print_string [ yellow ] "Press Enter to return to the main menu...";
     ignore (read_line ())

   and display_active_wallets () =
     print_string [ yellow ] "Your active wallets:\n";
     try
       let in_channel = open_in wallets_file in
       try
         while true do
           let address = input_line in_channel in
           print_string [ green ] (address ^ "\n")
         done
       with End_of_file -> close_in in_channel
     with Sys_error _ ->
       print_string [ red ] "No active wallets found.\n";
       print_string [ yellow ] "Press Enter to return to the main menu...";
       ignore (read_line ())

   and check_balance () =
     print_string [ yellow ] "Enter wallet address to check balance: ";
     let address = read_line () in
     Lwt_main.run (fetch_and_display_balance address);
     print_string [ yellow ] "Press Enter to return to the main menu...";
     ignore (read_line ())

   and remove_wallet () =
     print_string [ yellow ] "Enter wallet address to remove: ";
     let address_to_remove = read_line () in
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
       print_string [ red ] "Error removing wallet address.\n";
       print_string [ yellow ] "Press Enter to return to the main menu...";
       ignore (read_line ())

   let () =
     (* Start a background loop to update prices every 10 seconds *)
     let coins = [ "bitcoin"; "ethereum" ] in
     Lwt_main.run
       (let _ = Lwt.async (fun () -> fetch_and_display_prices_loop coins 10.0) in
        Lwt.return (main_menu ())) *)
