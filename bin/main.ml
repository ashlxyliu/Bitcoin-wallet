open ANSITerminal
open Final.Wallet
open Scraper

(* Define the filename for storing wallet addresses *)
let wallets_file = "wallets.txt"

(* Function to save a wallet address to a file *)
let save_wallet_address address =
  (* Open the file in append mode, creating it if it doesn't exist *)
  let out_channel =
    open_out_gen [ Open_append; Open_creat ] 0o666 wallets_file
  in
  (* Write the address to the file *)
  output_string out_channel (address ^ "\n");
  (* Close the file *)
  close_out out_channel

(* Function to print the main menu *)
let print_main_menu () =
  (* Print the welcome message *)
  print_string [ Foreground Cyan ] "Welcome to our Bitcoin wallet\n";
  (* Print the options *)
  print_string [ Foreground Cyan ] "Please select an option:\n";
  print_string [ Foreground Cyan ] "1. My Active Wallets\n";
  print_string [ Foreground Cyan ] "2. Create New Wallet\n";
  print_string [ Foreground Cyan ] "3. Fetch Coin Prices\n";
  print_string [ Foreground Cyan ] "4. Check Balance\n";
  print_string [ Foreground Cyan ] "5. Remove Wallet\n";
  print_string [ Foreground Cyan ] "6. Exit\n";
  (* Print the prompt for user input *)
  print_string [ Reset ] "> "

(* Recursive function to display and handle the main menu *)
let rec main_menu () =
  (* Print the main menu *)
  print_main_menu ();
  (* Read the user's input *)
  match read_line () with
  | "1" ->
      (* Display active wallets *)
      display_active_wallets ();
      (* Return to the main menu *)
      main_menu ()
  | "2" ->
      (* Create a new wallet *)
      create_new_wallet ();
      (* Return to the main menu *)
      main_menu ()
  | "3" ->
      (* Fetch and display cryptocurrency prices *)
      Lwt_main.run (fetch_and_display_prices ());
      (* Return to the main menu *)
      main_menu ()
  | "4" ->
      (* Check the balance of a wallet *)
      check_balance ();
      (* Return to the main menu *)
      main_menu ()
  | "5" ->
      (* Remove a wallet address *)
      remove_wallet ();
      (* Return to the main menu *)
      main_menu ()
  | "6" ->
      (* Print the exit message *)
      print_string [ green ] "Exiting... Goodbye!\n";
      (* Exit the program *)
      exit 0
  | _ ->
      (* Print an error message for invalid input *)
      print_string [ red ] "Invalid option, please try again.\n";
      (* Return to the main menu *)
      main_menu ()

(* Function to create a new wallet *)
and create_new_wallet () =
  (* Print the creation message *)
  print_string [ yellow ] "Creating a new wallet...\n";
  (* Generate a private key *)
  let sk = generate_private_key () in
  (* Generate a public key from the private key *)
  let pk = generate_public_key sk in
  (* Print the new wallet address *)
  print_string [ yellow ] ("Your new wallet address is: " ^ pk ^ "\n");
  (* Save the new wallet address *)
  save_wallet_address pk

(* Function to display active wallets *)
and display_active_wallets () =
  (* Print the active wallets message *)
  print_string [ yellow ] "Your active wallets:\n";
  (* Try to open the wallets file *)
  try
    let in_channel = open_in wallets_file in
    (* Read and print each wallet address *)
    try
      while true do
        let address = input_line in_channel in
        print_string [ green ] (address ^ "\n")
      done
    with End_of_file -> close_in in_channel
    (* Handle the case where the file does not exist *)
  with Sys_error _ -> print_string [ red ] "No active wallets found.\n"

(* Function to check the balance of a wallet *)
and check_balance () =
  (* Print the balance check prompt *)
  print_string [ yellow ] "Enter wallet address to check balance: ";
  (* Read the wallet address from the user *)
  let address = read_line () in
  (* Fetch and display the balance *)
  Lwt_main.run (fetch_and_display_balance address)

(* Function to remove a wallet address *)
and remove_wallet () =
  (* Print the removal prompt *)
  print_string [ yellow ] "Enter wallet address to remove: ";
  (* Read the address to remove from the user *)
  let address_to_remove = read_line () in
  (* Temporary file for storing remaining addresses *)
  let temp_file = "temp_wallets.txt" in
  (* Try to open the wallets file *)
  try
    let in_channel = open_in wallets_file in
    (* Open the temporary file for writing *)
    let out_channel = open_out temp_file in
    (* Read and write each address, skipping the one to remove *)
    try
      while true do
        let address = input_line in_channel in
        if address <> address_to_remove then
          output_string out_channel (address ^ "\n")
      done;
      (* Close the files *)
      close_in in_channel;
      close_out out_channel;
      (* Rename the temporary file to the original file name *)
      Sys.rename temp_file wallets_file;
      (* Print the success message *)
      print_string [ green ] "Wallet address removed successfully.\n"
    with End_of_file ->
      (* Close the files and rename the temporary file *)
      close_in in_channel;
      close_out out_channel;
      Sys.rename temp_file wallets_file;
      (* Print the success message *)
      print_string [ green ] "Wallet address removed successfully.\n"
    (* Handle errors during the file operations *)
  with Sys_error _ -> print_string [ red ] "Error removing wallet address.\n"

(* Entry point of the program *)
let () =
  (* Start the main menu *)
  main_menu ()
