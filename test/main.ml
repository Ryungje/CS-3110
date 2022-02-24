open OUnit2
open Game
open Cards
open Player
open Commands
open State

(**********************************************************************
  Add definitions and helper functions for test cases below.
  ********************************************************************)

(** [repeat f n x] is the output when function [f] is applied on input
    [x] for a total of [n] times. *)
let rec repeat f n x = if n <= 0 then x else x |> f |> repeat f (n - 1)

(** [cmp_lists lst1 lst2] compares two lists to see whether they are
    equivalent, meaning they must contain the same elements, though not
    necessarily in the same order. *)
let cmp_lists lst1 lst2 =
  let stack1 = List.sort compare lst1 in
  let stack2 = List.sort compare lst2 in
  List.length lst1 = List.length stack1
  && List.length lst2 = List.length stack2
  && stack1 = stack2

(** [shuffle_test name d expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [shuffle d]. *)

let shuffle_test (name : string) (d : deck) (expected_output : deck) :
    test =
  name >:: fun _ ->
  assert_equal true
    (cmp_lists (cards_of (shuffle d)) (cards_of expected_output))

(** [print_cards d num] prints out the name of the first [num]th cards
    in a deck to check if the order of the cards is correct. *)
let rec print_cards d num =
  if num = 0 then print_endline "end of deck"
  else
    match peek d with
    | n, _ ->
        let _ = print_endline n in
        print_cards (pop d) (num - 1)

(** [peek_test name d expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [peek d]. *)
let peek_test
    (name : string)
    (d : Cards.deck)
    (expected_output : string * int) : test =
  name >:: fun _ -> assert_equal expected_output (peek d)

(** [pop_test name d expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [peak (pop d)]. *)
let pop_test
    (name : string)
    (d : Cards.deck)
    (expected_output : string * int) : test =
  name >:: fun _ -> assert_equal expected_output (pop d |> peek)

(** [pop_newdeck_test name d expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [pop d]. *)
let pop_newdeck_test
    (name : string)
    (d : Cards.deck)
    (expected_output : Cards.deck) : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_lists (cards_of expected_output) (cards_of (pop d)))

(** [player_test name p expected_output] constructs an OUnit test named
    [name] that asserts the quality [expected_output] with ([name_of p],
    [show_hand p], [hand_value p])*)
let player_test
    (name : string)
    (p : Player.player)
    (expected_output : string * string list * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (name_of p, show_hand p, hand_value p)

(** [parse_number_test name i expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [parse_number i]*)
let parse_number_test
    (name : string)
    (i : string)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (parse_number i)

(** [parse_number_exception_test name i e] constructs an OUnit test
    named [name] that asserts an exception [e] is raised with
    [parse_number i]. *)
let parse_number_exception_test (name : string) (i : string) e : test =
  name >:: fun _ ->
  let f () = parse_number i in
  assert_raises e f

(** [parse_name_test name n n_list expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [parse_name n n_list]*)
let parse_name_test
    (name : string)
    (n : string)
    (n_list : string list)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (parse_name n n_list)

(** [parse_name_exception_test name n n_list e] constructs an OUnit test
    named [name] that asserts an exception [e] is raised with
    [parse_name n n_list]. *)
let parse_name_exception_test
    (name : string)
    (n : string)
    (n_list : string list)
    e : test =
  name >:: fun _ ->
  let f () = parse_name n n_list in
  assert_raises e f

(** [parse_command_test name str expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [parse_command str]*)
let parse_command_test
    (name : string)
    (str : string)
    (expected_output : Commands.command) : test =
  name >:: fun _ -> assert_equal expected_output (parse_command str)

(** [parse_command_exception_test name str e] constructs an OUnit test
    named [name] that asserts an exception [e] is raised with
    [parse_command str]. *)
let parse_command_exception_test (name : string) (str : string) e : test
    =
  name >:: fun _ ->
  let f () = parse_command str in
  assert_raises e f

(** [init_state_test name num_deck num_player player_names] constructs
    an OUnit test named [name] that tests
    [init_state num_deck num_player player_names]. It checks if the
    total number of cards in player and dealer hands is equal to
    2*(#ofplayer)+1 and checks if number of cards remaining in the deck
    is (num_deck*52)-2*(#ofplayer)-1. *)
let rec count p_list acc =
  match p_list with
  | [] -> acc
  | h :: t -> (
      match h with
      | n, cards -> count t (acc + List.length cards))

let init_state_test
    (name : string)
    (num_deck : int)
    (num_player : int)
    (player_names : string list) : test =
  name >:: fun _ ->
  assert_equal true
    (let st = init_state num_deck num_player player_names in
     let plist = list_of_players st in
     let num_cards =
       count plist 0 + List.length (st |> dealer_of |> show_hand)
     in
     let num_in_deck =
       st |> remaining_deck |> cards_of |> List.length
     in
     num_cards + num_in_deck = (num_deck * 52) - 1)

(** [init_state_exception_test name num_deck num_player player_names e]
    constructs an OUnit test named [name] that asserts an exception [e]
    is raised with [init_state num_deck num_player player_names]. *)
let init_state_exception_test
    (name : string)
    (num_deck : int)
    (num_player : int)
    (player_names : string list)
    e : test =
  name >:: fun _ ->
  let f () = init_state num_deck num_player player_names in
  assert_raises e f

(** [state_hiddencard_test name num_deck st] constructs an OUnit test
    named [name] that tests if the total number of cards in player and
    dealers hands of [st] is equal to the number of cards taken from the
    deck (num_deck*52 - length_of_deck). Requires: [st] is a valid game
    state and the dealer currently had a hidden card. *)

let state_hiddencard_test
    (name : string)
    (num_deck : int)
    (st0 : State.s) : test =
  name >:: fun _ ->
  assert_equal true
    (let st = st0 in
     let plist = list_of_players st in
     let num_cards =
       count plist 0 + List.length (st |> dealer_of |> show_hand)
     in
     let num_in_deck =
       st |> remaining_deck |> cards_of |> List.length
     in
     num_cards + num_in_deck = (num_deck * 52) - 1)

(** [state_completedealer_test name num_deck st] constructs an OUnit
    test named [name] that tests if the total number of cards in player
    and dealers hands of [st] is equal to the number of cards taken from
    the deck (num_deck*52 - length_of_deck). Requires: [st] is a valid
    game state and the dealer has no hidden cards and completed his hand
    to be greater than 17. *)

let state_completedealer_test
    (name : string)
    (num_deck : int)
    (st0 : State.s) : test =
  name >:: fun _ ->
  assert_equal true
    (let st = st0 in
     let plist = list_of_players st in
     let num_cards =
       count plist 0 + List.length (st |> dealer_of |> show_hand)
     in
     let num_in_deck =
       st |> remaining_deck |> cards_of |> List.length
     in
     num_cards + num_in_deck = num_deck * 52
     && st |> dealer_of |> hand_value > 17)

(** [print_players p_list] prints the name and hand of each player in
    [p_list] to check if state functions are working. *)
let rec print_players p_list =
  match p_list with
  | [] -> print_string ""
  | h :: t -> (
      match h with
      | name, cards ->
          let _ =
            print_endline (name ^ "'s hand: " ^ String.concat ", " cards)
          in
          print_players t)

(** [print_dealer d_hand] prints the hand of the dealer to check if
    state functions are working. *)
let print_dealer d_hand =
  print_endline ("Dealer's hand: " ^ String.concat ", " d_hand)

(**********************************************************************
  Add unit tests for modules below.
  ********************************************************************)

(* Sample card deck *)
let card_deck = reset 1
let one_card_deck = repeat pop 51 (reset 1)

(* Sample players *)
let p0 = init_stats "Bob Carlos"

let p1 =
  p0
  |> add_card ("Five of Hearts", 5)
  |> add_card ("Queen of Spades", 10)

let p_none = reset_hand p1

(* Sample dealer *)
let d_with_hidden =
  init_stats "Dealer"
  |> add_card ("Two of Clubs", 2)
  |> add_hidden ("Nine of Diamonds", 9)

let d_revealed = reveal d_with_hidden

let _ =
  print_cards (shuffle card_deck) (List.length (cards_of card_deck))

let cards_tests =
  [
    peek_test "Testing peek" card_deck ("Ace of Clubs", 1);
    pop_test "Testing pop" card_deck ("Two of Clubs", 2);
    pop_newdeck_test "Testing pop on deck with one card" one_card_deck
      card_deck;
    shuffle_test "Testing shuffle" card_deck card_deck;
  ]

let player_tests =
  [
    player_test "Player with no hand" p0 ("Bob Carlos", [], 0);
    player_test "Player with a hand" p1
      ("Bob Carlos", [ "Five of Hearts"; "Queen of Spades" ], 15);
    player_test "Reset player's hand" p_none ("Bob Carlos", [], 0);
    player_test "Dealer with hidden card" d_with_hidden
      ("Dealer", [ "Two of Clubs" ], 2);
    player_test "Dealer revealed hidden card" d_revealed
      ("Dealer", [ "Two of Clubs"; "Nine of Diamonds" ], 11);
  ]

let command_tests =
  [
    parse_number_test "Parse valid integer" "1" 1;
    parse_number_test "Parse valid integer with spaces" "   9   " 9;
    parse_number_exception_test "Parse invalid input 0" "0" Malformed;
    parse_number_exception_test "Parse invalid input of negative number"
      "-2" Malformed;
    parse_number_exception_test
      "Parse invalid input with too many values" "  10 11" Malformed;
    parse_number_exception_test "Parse invalid input with words"
      " 13 hell" Malformed;
    parse_number_exception_test "Parse invalid float number" "19.2"
      Malformed;
    parse_number_exception_test "Parse empty input" "" Empty;
    parse_number_exception_test "Parse space only input" "    " Empty;
    parse_name_test "Parse valid name: Bob" "  Bob  " [] [ "Bob" ];
    parse_name_test "Parse valid name : Henry Conlon" "Henry   Conlon"
      [ "Bob" ]
      [ "Bob"; "Henry Conlon" ];
    parse_name_exception_test "Parse empty name" "" [] Empty;
    parse_name_exception_test "Parse space only name" "    " [ "Bob" ]
      Empty;
    parse_name_exception_test "Parse name already in list" "Bob"
      [ "Henry"; "Bob" ] Malformed;
    parse_command_test "Parse hit command" "  hit " Hit;
    parse_command_test "Parse stand command" "stand " Stand;
    parse_command_test "Parse play command" "  play" Play;
    parse_command_test "Parse quit command" "quit" Quit;
    parse_command_exception_test "Parse empty command" "" Empty;
    parse_command_exception_test "Parse space only command" "    " Empty;
    parse_command_exception_test "Parse invalid command for hit"
      "hit me" Malformed;
    parse_command_exception_test "Parse invalid command for stand"
      "stand up" Malformed;
    parse_command_exception_test "Parse invalid command for play"
      "play again" Malformed;
    parse_command_exception_test "Parse invalid command for quit"
      "quit game now" Malformed;
    parse_command_exception_test
      "Parse invalid and unrecognized command" "open seasame" Malformed;
  ]

let st0 = init_state 2 3 [ "Bob"; "Alice"; "Henry" ]
let _ = print_endline "Initial state"
let _ = print_players (list_of_players st0)
let _ = print_dealer (st0 |> dealer_of |> show_hand)
let st1 = deal "Alice" st0
let _ = print_endline "Alice gets another card"
let _ = print_players (list_of_players st1)
let _ = print_dealer (st1 |> dealer_of |> show_hand)
let st2 = complete_hand st1
let _ = print_endline "Dealer completes his hand"
let _ = print_players (list_of_players st2)
let _ = print_dealer (st2 |> dealer_of |> show_hand)

let state_tests =
  [
    init_state_test "test initial state" 2 3 [ "Bob"; "Alice"; "Henry" ];
    init_state_exception_test
      "test that init_state raises InvalidInput exception" 2 4
      [ "Bob"; "Alice"; "Henry" ]
      InvalidInput;
    state_hiddencard_test "test dealing a card to a player" 2 st1;
    state_completedealer_test "test dealer completes hand" 2 st2;
  ]

let suite =
  "test suite for BlackJack"
  >::: List.flatten
         [ cards_tests; player_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite