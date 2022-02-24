open Game
open Cards
open Player
open Commands
open State

let start_game num_deck num_players player_names = ()

let rec get_num_decks nplayers name_list =
  let _ =
    print_string "How many decks of cards will be used? ";
    match parse_number (read_line ()) with
    | exception End_of_file -> ()
    | exception Escape -> exit 0
    | exception _ ->
        print_endline "Invalid integer! Try again.";
        get_num_decks nplayers name_list
    | i -> start_game i nplayers name_list
  in
  ()

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
      | list -> get_player_names n list (acc + 1)
    in
    ()
  else get_num_decks n n_list

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
    "\n\nWelcome to OCasino.\n";
  num_players ()

(* Execute the game engine. *)
let () = main ()