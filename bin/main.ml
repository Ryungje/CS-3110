open Game
open Cards
open Player
open Commands
open State

(* Step 4c: see if players want to play again or quit *)
let rec game_again _ =
  print_string "Would you like to [play] again or [quit]? ";
  match parse_command (read_line ()) with
  | exception End_of_file -> false
  | exception Escape -> false
  | exception _ ->
      print_endline "Invalid command! Try again.";
      game_again ()
  | Play -> true
  | _ ->
      print_endline "Invalid command! Try again.";
      game_again ()

(* Step 4b: print out results *)
let rec print_results plist dvalue =
  match plist with
  | [] -> ()
  | h :: t ->
      let _ =
        if dvalue > 21 || (hand_value h > dvalue && hand_value h <= 21)
        then print_endline (name_of h ^ " won!")
        else
          (*think about this later: if they tie, the person with less
            cards wins and if they have the same number of cards, then
            bet just returns to the player*)
          print_endline (name_of h ^ " lost!")
      in
      print_results t dvalue

(* Step 4a: players give commands to complete hand *)
let rec get_player_command st plist n =
  if n < List.length plist then (
    let p = List.nth plist n in
    print_endline
      (name_of p ^ "'s hand: " ^ String.concat ", " (show_hand p));
    print_endline "Choices: hit, stand";
    print_string "> ";
    match parse_command (read_line ()) with
    | exception End_of_file -> st
    | exception Escape -> exit 0
    | Stand ->
        print_newline ();
        get_player_command st plist (n + 1)
    | Hit ->
        let new_st = deal (name_of p) st in
        let updated_p = List.nth (players_of new_st) n in
        if is_bust updated_p then
          let _ =
            print_endline
              (name_of updated_p ^ "'s hand: "
              ^ String.concat ", " (show_hand updated_p));
            print_endline (name_of updated_p ^ " busted!\n")
          in
          get_player_command new_st plist (n + 1)
        else get_player_command new_st (players_of new_st) n
    | exception _ ->
        print_endline "Invalid command! Try again.";
        get_player_command st plist n
    | Play ->
        print_endline "Invalid command! Try again.";
        get_player_command st plist n)
  else st

(* Step 4: keep playing rounds of blackjack until players want to
   quit *)
let rec play_game st =
  let _ =
    print_endline "\n\nShuffling cards... Dealing to players... ";
    print_endline
      ("Dealer's hand: "
      ^ String.concat ", " (st |> dealer_of |> show_hand)
      ^ "\n");
    let new_st = get_player_command st (players_of st) 0 in
    let end_st = complete_hand new_st in
    print_endline "Completing Dealer's hand...";
    print_endline
      ("Dealer's hand: "
      ^ String.concat ", " (end_st |> dealer_of |> show_hand));
    if end_st |> dealer_of |> hand_value > 21 then
      print_endline "Dealer busted!"
    else ();
    print_newline ();
    print_endline "Results: ";
    print_results (players_of end_st) (end_st |> dealer_of |> hand_value);
    print_newline ();
    if game_again () then play_game (reset_all end_st) else exit 0
  in
  ()

(* Step 3: get the number of decks that will be used *)
let rec get_num_decks nplayers name_list =
  let _ =
    print_string "How many decks of cards will be used? ";
    match parse_number (read_line ()) with
    | exception End_of_file -> ()
    | exception Escape -> exit 0
    | exception _ ->
        print_endline "Invalid integer! Try again.";
        get_num_decks nplayers name_list
    | i ->
        let st0 = init_state i nplayers name_list in
        play_game st0
  in
  ()

(* Step 2: get names of all the players *)
let rec get_player_names n n_list acc =
  if n <> acc then
    let _ =
      print_string ("Name of Player " ^ string_of_int (acc + 1) ^ ": ");
      match parse_name (read_line ()) n_list with
      | exception End_of_file -> ()
      | exception Escape -> exit 0
      | exception _ ->
          print_endline "Invalid name! Try again.";
          get_player_names n n_list acc
      | new_list -> get_player_names n new_list (acc + 1)
    in
    ()
  else get_num_decks n (List.rev n_list)

(* Step 1: get number of players that will be participating *)
let rec num_players _ =
  let _ =
    print_string "How many players are participating? ";
    match parse_number (read_line ()) with
    | exception End_of_file -> ()
    | exception Escape -> exit 0
    | exception _ ->
        print_endline "Invalid integer! Try again.";
        num_players ()
    | i -> get_player_names i [] 0
  in
  ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to OCasino's BlackJack.\n";
  num_players ()

(* Execute the game engine. *)
let () = main ()