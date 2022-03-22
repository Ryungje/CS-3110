open Game
open Cards
open Player
open Commands
open State

(* helper functions *)
let remove_last lst = lst |> List.rev |> List.tl

(* Step 4g: see if players want to play again or quit *)
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

(* Step 4fi: help updated player total by (+) if player won or by (-) if
   player lost *)
let update_player_total operator pname st =
  let new_st = redeem_bet operator pname st in
  let new_player =
    List.filter (fun x -> name_of x = pname) (players_of new_st)
  in
  (List.hd new_player, new_st)

(* Step 4f: print out results after a full round *)
let rec print_results plist d st =
  match plist with
  | [] -> st
  | h :: t ->
      let dvalue = hand_value d in
      let snd_h = switch_hands h in
      let str = name_of h ^ "'s remaining chips: " in
      if
        dvalue > 21
        || (hand_value h > dvalue && hand_value h <= 21)
        || hand_value h = dvalue
           && List.length (show_hand h) < List.length (show_hand d)
        || (hand_value snd_h > dvalue && hand_value snd_h <= 21)
        || hand_value snd_h = dvalue
           && List.length (show_hand snd_h) < List.length (show_hand d)
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
        || hand_value snd_h = dvalue
           && List.length (show_hand snd_h) = List.length (show_hand d)
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

(* Step 4di: figure out which choices players can select *)
let rec build_choices p choice_str acc =
  match (acc, choice_str) with
  | 0, h :: t ->
      if has_double p && not (has_snd_hand p) then
        h :: build_choices p t (acc + 1)
      else build_choices p t (acc + 1)
  | 1, h :: t ->
      if has_pair p && not (has_snd_hand p) then
        h :: build_choices p t (acc + 1)
      else build_choices p t (acc + 1)
  | 2, h :: t -> h :: build_choices p t (acc + 1)
  | 3, h :: t -> h :: build_choices p t (acc + 1)
  | 4, h :: t ->
      if has_ace p then h :: build_choices p t (acc + 1)
      else build_choices p t (acc + 1)
  | _, _ -> []

(* Step 4d: players give commands to complete hand *)
let rec get_player_command st plist n_plyr swapped =
  if n_plyr < List.length plist then (
    let p = List.nth (players_of st) n_plyr in
    (* print out player's current stats *)
    if not (has_snd_hand p) then (
      print_endline
        ("Completing " ^ name_of p ^ "'s hand: "
        ^ String.concat ", " (show_hand p));
      card_display (show_hand p))
    else if has_snd_hand p && not swapped then
      print_endline
        ("Completing " ^ name_of p ^ "'s left hand: "
        ^ String.concat ", " (show_hand p))
    else
      print_endline
        ("Completing " ^ name_of p ^ "'s right hand: "
        ^ String.concat ", " (show_hand p));
    print_endline ("Current bet: " ^ string_of_int (current_bet p));
    print_endline
      ("Remaining chips: "
      ^ string_of_int (current_total p - current_bet p));
    (* print out commands that player could choose from *)
    let choice_str =
      [ "double down"; "split"; "hit"; "stand"; "ace to eleven" ]
    in
    let new_str = build_choices p choice_str 0 in
    print_endline ("Choices: " ^ String.concat ", " new_str);
    (* get player's input *) print_string "> ";
    match parse_command (read_line ()) with
    | exception End_of_file -> st
    | exception Escape -> exit 0
    | Stand ->
        if p |> switch_hands |> hand_value = 0 || swapped then
          let _ = print_newline () in
          get_player_command st plist (n_plyr + 1) false
        else
          let new_st = swap_hand (name_of p) st in
          get_player_command new_st plist n_plyr true
    | Hit ->
        let new_st = deal (name_of p) st in
        let updated_p = List.nth (players_of new_st) n_plyr in
        if is_bust updated_p then
          let _ =
            print_endline
              (name_of updated_p ^ "'s hand: "
              ^ String.concat ", " (show_hand updated_p));
            print_endline (name_of updated_p ^ " busted!\n")
          in
          get_player_command new_st plist (n_plyr + 1) swapped
        else
          get_player_command new_st (players_of new_st) n_plyr swapped
    | Split ->
        if List.length (show_hand p) = 2 && has_pair p then
          let new_st = split_hand (name_of p) st in
          get_player_command new_st (players_of new_st) n_plyr swapped
        else
          let _ = print_endline "Cannot split cards! Try again." in
          get_player_command st plist n_plyr swapped
    | AceToEleven ->
        let new_st = change_ace (name_of p) st in
        get_player_command new_st plist n_plyr swapped
    | DoubleDown ->
        if has_double p && not (has_snd_hand p) then
          let new_st =
            st |> double_down (name_of p) |> deal (name_of p)
          in
          let updated_p = List.nth (players_of new_st) n_plyr in
          if is_bust updated_p then
            let _ =
              print_endline
                (name_of updated_p ^ "'s hand: "
                ^ String.concat ", " (show_hand updated_p));
              print_endline (name_of updated_p ^ " busted!\n")
            in
            get_player_command new_st plist (n_plyr + 1) swapped
          else
            get_player_command new_st (players_of new_st) n_plyr swapped
        else
          let _ = print_endline "Cannot double down! Try again." in
          get_player_command st plist n_plyr swapped
    | exception _ ->
        print_endline "Invalid command! Try again.";
        get_player_command st plist n_plyr swapped
    | Play ->
        print_endline "Invalid command! Try again.";
        get_player_command st plist n_plyr swapped)
  else st

(* Step 4e: print out results after natural (s) *)
let rec print_results_from_natural plist =
  match plist with
  | [] -> ()
  | h :: t ->
      let str = name_of h ^ "'s remaining chips: " in
      print_endline (str ^ string_of_int (current_total h));
      print_results_from_natural t

(* Step 4c: handle the scenario where one of the players or the dealer
   has a natural *)
let handle_naturals st =
  if not (is_dealer_natural (dealer_of st)) then
    (* dealer does not have natural so pay the players with a natural *)
    unnatural_dealer_natural_player st
  else natural_dealer_unnatural_player st

(* Step 4b: print out starting two cards of each player *)
let rec print_player_starting_cards st plist n =
  if n < List.length plist then (
    let p = List.nth (players_of st) n in
    print_endline
      (name_of p ^ "'s hand: " ^ String.concat ", " (show_hand p));
    if is_natural p then
      let _ = print_endline (name_of p ^ " has a natural!") in
      print_player_starting_cards st plist (n + 1)
    else print_player_starting_cards st plist (n + 1))
  else ()

(* Step 4a: get starting bets of all players at beginning of each
   round *)
let rec get_player_bets st acc =
  if acc = List.length (players_of st) then st
  else
    let p = List.nth (players_of st) acc in
    print_string ("Starting bet for " ^ name_of p ^ ": ");
    match parse_number (read_line ()) with
    | exception End_of_file -> st
    | exception Escape -> exit 0
    | exception _ ->
        print_endline "Invalid integer! Try again.";
        get_player_bets st acc
    | i ->
        let new_st = increase_bet i (name_of p) st in
        get_player_bets new_st (acc + 1)

(* Step 4: keep playing rounds of blackjack until players want to
   quit *)
let rec play_game num_rounds st =
  let _ =
    print_endline ("\nRound " ^ string_of_int num_rounds ^ ":");
    let bet_st = get_player_bets st 0 in
    print_endline "\nShuffling cards... Dealing to players... ";
    if is_dealer_natural (bet_st |> dealer_of) then (
      print_endline
        ("Dealer's hand: "
        ^ String.concat ", " (bet_st |> dealer_of |> reveal |> show_hand)
        );
      print_endline "Dealer has a natural!")
    else
      print_endline
        ("Dealer's hand: "
        ^ String.concat ", " (bet_st |> dealer_of |> show_hand));
    print_player_starting_cards bet_st (players_of bet_st) 0;
    print_newline ();
    if
      List.filter is_natural (players_of bet_st) |> List.length <> 0
      || is_dealer_natural (bet_st |> dealer_of)
    then (
      let new_st = handle_naturals bet_st in
      let end_st = reset_all new_st in
      print_results_from_natural (players_of end_st);
      let _ = print_newline () in
      if game_again () then play_game (num_rounds + 1) end_st
      else exit 0)
    else
      let new_st = get_player_command bet_st (players_of st) 0 false in
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
      if game_again () then
        play_game (num_rounds + 1) (reset_all final_st)
      else exit 0
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
        print_newline ();
        play_game 1 st0
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