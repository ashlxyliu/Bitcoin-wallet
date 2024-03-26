open ANSITerminal

let print_main_menu () =
  print_string [ Foreground Cyan ] "Welcome to our Bitcoin wallet\n";
  print_string [ Foreground Cyan ] "Please select an option:\n";
  print_string [ Foreground Cyan ] "1. My Active Wallets\n";
  print_string [ Foreground Cyan ] "2. Create New Wallet\n";
  print_string [ Foreground Cyan ] "3. Exit\n";
  print_string [ Reset ] "> "

let rec main_menu () =
  print_main_menu ();
  match read_line () with
  | "1" ->
      print_string [ red ] "Functionality not implemented yet.\n";
      main_menu ()
  | "2" -> create_new_wallet ()
  | "3" ->
      print_string [ green ] "Exiting... Goodbye!\n";
      exit 0
  | _ ->
      print_string [ red ] "Invalid option, please try again.\n";
      main_menu ()

and create_new_wallet () =
  print_string [ yellow ] "Creating a new wallet...\n";
  (* This is where you would add your wallet creation and key generation logic. *)
  print_string [ yellow ] "New wallet created successfully!\n";
  main_menu ()

let () = main_menu ()
