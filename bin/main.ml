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

(* Step 4bi: help updated player total by (+) if player won or by (-) if
   player lost *)
let update_player_total operator pname st =
  let new_st = redeem_bet operator pname st in
  let new_player =
    List.filter (fun x -> name_of x = pname) (players_of new_st)
  in
  (List.hd new_player, new_st)

(* Step 4b: print out results *)
let rec print_results plist d st =
  match plist with
  | [] -> st
  | h :: t ->
      let dvalue = hand_value d in
      let str = name_of h ^ "'s remaining chips: " in
      if
        dvalue > 21
        || (hand_value h > dvalue && hand_value h <= 21)
        || hand_value h = dvalue
           && List.length (show_hand h) < List.length (show_hand d)
      then
        (* player won *)
        let new_player, new_st =
          update_player_total ( + ) (name_of h) st
        in
        let _ =
          print_endline (str ^ string_of_int (current_total new_player))
        in
        print_results t d new_st
      else if
        hand_value h = dvalue
        && List.length (show_hand h) = List.length (show_hand d)
      then
        (* player tied *)
        let _ = print_endline (str ^ string_of_int (current_total h)) in
        print_results t d st
      else
        (* player lost *)
        let new_player, new_st =
          update_player_total ( - ) (name_of h) st
        in
        let _ =
          print_endline (str ^ string_of_int (current_total new_player))
        in
        print_results t d new_st

(* Step 4a: players give commands to complete hand *)
let rec get_player_command st plist n =
  if n < List.length plist then (
    let p = List.nth (players_of st) n in
    print_endline
      (name_of p ^ "'s hand: " ^ String.concat ", " (show_hand p));
    print_endline ("Current bet: " ^ string_of_int (current_bet p));
    print_endline
      ("Remaining chips: "
      ^ string_of_int (current_total p - current_bet p));
    print_endline "Choices: hit, stand, bet";
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
    | Bet i ->
        let new_st = increase_bet i (name_of p) st in
        get_player_command new_st plist n
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
    let final_st =
      print_results (players_of end_st) (end_st |> dealer_of) end_st
    in
    print_newline ();
    if game_again () then play_game (reset_all final_st) else exit 0
  in
  ()

(* Step 3: get the number of decks that will be used *)
let rec get_num_decks nplayers name_list chips_list =
  let _ =
    print_string "How many decks of cards will be used? ";
    match parse_number (read_line ()) with
    | exception End_of_file -> ()
    | exception Escape -> exit 0
    | exception _ ->
        print_endline "Invalid integer! Try again.";
        get_num_decks nplayers name_list chips_list
    | i ->
        let st0 = init_state i nplayers name_list chips_list in
        play_game st0
  in
  ()

(* Step 2b: get starting chips for player n *)
let rec get_starting_chips n =
  print_string
    ("Starting number of chips for Player " ^ string_of_int n ^ ": ");
  match parse_number (read_line ()) with
  | exception End_of_file -> get_starting_chips n
  | exception Escape -> exit 0
  | exception _ ->
      print_endline "Invalid integer! Try again.";
      get_starting_chips n
  | i -> i

(* Step 2: get names and starting chips of all the players *)
let rec get_players n n_list b_list acc =
  if n <> acc then
    let _ =
      print_string ("Name of Player " ^ string_of_int (acc + 1) ^ ": ");
      match parse_name (read_line ()) n_list with
      | exception End_of_file -> ()
      | exception Escape -> exit 0
      | exception _ ->
          print_endline "Invalid name! Try again.";
          get_players n n_list b_list acc
      | new_list ->
          let chips = get_starting_chips (acc + 1) in
          get_players n new_list (b_list @ [ chips ]) (acc + 1)
    in
    ()
  else get_num_decks n n_list b_list

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
    | i -> get_players i [] [] 0
  in
  ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to OCasino's BlackJack.\n";
  num_players ()

(* Execute the game engine. *)
let () = main ()