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
    [show_hand p], [hand_value p], [is_bust p], [current_bet p],
    [current_total p])*)
let player_test
    (name : string)
    (p : Player.player)
    (expected_output : string * string list * int * bool * int * int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    ( name_of p,
      show_hand p,
      hand_value p,
      is_bust p,
      current_bet p,
      current_total p )

let player_switchhand_test
    (name : string)
    (p : Player.player)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (p |> switch_hands |> show_hand)

let player_hassndhand_test
    (name : string)
    (p : Player.player)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (p |> has_snd_hand)

let player_hasdouble_test
    (name : string)
    (p : Player.player)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (has_double p)

let player_insurance_test
    (name : string)
    (amount : int)
    (operator : int -> int -> int)
    (p : Player.player)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (p |> add_insurance amount
    |> redeem_for_insurance operator
    |> current_total)

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
    (player_names : string list)
    (bet_list : int list) : test =
  name >:: fun _ ->
  assert_equal true
    (let st = init_state num_deck num_player player_names bet_list in
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
    (bet_list : int list)
    e : test =
  name >:: fun _ ->
  let f () = init_state num_deck num_player player_names bet_list in
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
     && st |> dealer_of |> hand_value >= 17)

(** [state_resetall_test name st] constructs an OUnit test named [name]
    that asserts that there are no cards in all player's and the
    dealer's hands. *)

let state_resetall_test (name : string) (st0 : State.s) : test =
  name >:: fun _ ->
  let st = st0 in
  let plist = list_of_players st in
  let ncards =
    count plist 0 + List.length (st |> dealer_of |> show_hand)
  in
  let nplayers = List.length plist in
  assert_equal ncards ((2 * nplayers) + 1)

let redeem_for_natural_test
    (name : string)
    (b : bool)
    (p : Player.player)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal
    ( current_bet (redeem_for_natural b p),
      current_total (redeem_for_natural b p) )
    expected_output

let has_ace_test
    (name : string)
    (p : Player.player)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal (has_ace p) expected_output

let has_pair_test
    (name : string)
    (p : Player.player)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal (has_pair p) expected_output

let split_test (name : string) (p : Player.player) expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    ( p |> split_pair |> show_hand,
      p |> split_pair |> switch_hands |> show_hand )

let get_bet_tup (st : State.s) =
  ( st |> players_of |> List.map current_bet,
    st |> players_of |> List.map current_total )

let state_increasebet_test
    (name : string)
    (amount : int)
    (pname : string)
    (st : State.s)
    (expected_output : int list * int list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (increase_bet amount pname st |> get_bet_tup)

let state_redeembet_test
    (name : string)
    (operator : int -> int -> int)
    (pname : string)
    (st : State.s)
    (expected_output : int list * int list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (redeem_bet operator pname st |> get_bet_tup)

let get_insurance (st : State.s) =
  st |> players_of |> List.map current_total

let state_insurance_test
    (name : string)
    (amount : int)
    (operator : int -> int -> int)
    (pname : string)
    (st : State.s)
    (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (st
    |> increase_insurance amount pname
    |> redeem_insurance operator pname
    |> get_insurance)

(** [current_bet_test name p expected_output] constructs an OUnit test
    named [name] that asserts the current_bet of the player [p] is the
    same as [expected_output]*)
let current_bet_test
    (name : string)
    (p : player)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (current_bet p)

(** [current_total_test name p expected_output] constructs an OUnit test
    named [name] that asserts the current_total of the player [p] is the
    same as [expected_output]*)
let current_total_test
    (name : string)
    (p : player)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (current_total p)

(** [natural_test name p expected_output] constructs an OUnit test named
    [name] that asserts the hand of the player [p] is the a natural
    using the is_natural and compares output to [expected_output]*)
let natural_test (name : string) f (p : player) (expected_output : bool)
    : test =
  name >:: fun _ -> assert_equal expected_output (f p)

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
let p0bet = p0 |> add_bet 20 |> add_bet 15
let p0collect = p0bet |> redeem ( + )
let p0pay = p0bet |> redeem ( - )

let p1 =
  p0
  |> add_card ("Five of Hearts", 5)
  |> add_card ("Queen of Spades", 10)

let p4 =
  p0
  |> add_card ("Ace of Hearts", 1)
  |> add_card ("Ten of Spades", 10)
  |> add_bet 50

let player_only_ace = p0 |> add_card ("Ace of Hearts", 1)

let player_all_aces =
  player_only_ace
  |> add_card ("Ace of Spades", 1)
  |> add_card ("Ace of Clubs", 1)
  |> add_card ("Ace of Diamonds", 1)

let p5 = p1 |> add_bet 50
let p6 = p5 |> redeem ( + ) |> add_bet 50
let p7 = p4 |> redeem ( + ) |> add_bet 50

let pnatural_jack =
  p0 |> add_card ("Jack of Hearts", 10) |> add_card ("Ace of Spades", 1)

let pnatural_king =
  p0 |> add_card ("King of Clubs", 10) |> add_card ("Ace of Clubs", 1)

let p1betredeem =
  p1 |> add_bet 10 |> redeem ( + ) |> add_bet 20 |> redeem ( - )
  |> add_bet 5

(* natural *)
let p2 =
  p0 |> add_card ("Ace of Spades", 5) |> add_card ("Queen of Hearts", 5)

(* unnatural *)
let p3 =
  p0 |> add_card ("Four of Spades", 5) |> add_card ("Ten of Clubs", 5)

let p_none = reset_hand p1

let p_2pair =
  p0 |> add_card ("Two of Diamonds", 2) |> add_card ("Two of Hearts", 2)

let p_kingpair =
  p0
  |> add_card ("King of Hearts", 10)
  |> add_card ("King of Spades", 10)

let p_2split = p_2pair |> split_pair

let p_double =
  p0 |> add_card ("Five of Hearts", 5) |> add_card ("Four of Spades", 4)

let p8 =
  p0
  |> add_card ("Queen of Spades", 10)
  |> add_card ("Jack of Diamonds", 10)

(* Sample dealers *)

(* this one has unnatural hand *)
let d_with_hidden =
  init_stats "Dealer"
  |> add_card ("Three of Clubs", 3)
  |> add_hidden ("Nine of Diamonds", 9)

let d_revealed = reveal d_with_hidden
let d_busted = d_revealed |> add_card ("Queen of Hearts", 10)

(* this one has natural hand *)
let d_hidden_natural =
  init_stats "Dealer"
  |> add_card ("Ace of Spade", 5)
  |> add_hidden ("Ten of Clubs", 5)

let d_revealed_natural = reveal d_hidden_natural

let _ =
  print_cards (shuffle card_deck) (List.length (cards_of card_deck))

(* Test Cases *)
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
    player_test "Player with no hand" p0
      ("Bob Carlos", [], 0, false, 0, 0);
    player_test "Player with a hand" p1
      ( "Bob Carlos",
        [ "Five of Hearts"; "Queen of Spades" ],
        15,
        false,
        0,
        0 );
    player_test "Reset player's hand" p_none
      ("Bob Carlos", [], 0, false, 0, 0);
    player_test "Dealer with hidden card" d_with_hidden
      ("Dealer", [ "Three of Clubs" ], 3, false, 0, 0);
    player_test "Dealer revealed hidden card" d_revealed
      ( "Dealer",
        [ "Three of Clubs"; "Nine of Diamonds" ],
        12,
        false,
        0,
        0 );
    player_test "Dealer busted" d_busted
      ( "Dealer",
        [ "Three of Clubs"; "Nine of Diamonds"; "Queen of Hearts" ],
        22,
        true,
        0,
        0 );
    player_test "Player with no hand increases bet" p0bet
      ("Bob Carlos", [], 0, false, 35, 0);
    player_test "Player with no hand collects bet" p0collect
      ("Bob Carlos", [], 0, false, 0, 35);
    player_test "Player with no hand pays bet" p0pay
      ("Bob Carlos", [], 0, false, 0, -35);
    player_test "Player with a hand" p1betredeem
      ( "Bob Carlos",
        [ "Five of Hearts"; "Queen of Spades" ],
        15,
        false,
        5,
        -10 );
    redeem_for_natural_test "Player with natural and inital total 0"
      true p4 (0, 75);
    redeem_for_natural_test "Player with no natural and intial total 0"
      false p5 (0, 0);
    redeem_for_natural_test "Player with no natural and intial total 50"
      false p6 (0, 50);
    redeem_for_natural_test "Player with  natural and intial total 50"
      true p7 (0, 125);
    natural_test "p2 has natural hand w/ Queen" is_natural p2 true;
    natural_test "p4 has a natural hand w/ Ten" is_natural p4 true;
    natural_test "pnatural_jack has a natural hand w/ Jack" is_natural
      pnatural_jack true;
    natural_test "pnatural_king has a natural hand w/ King" is_natural
      pnatural_king true;
    natural_test "P1 does not have natural hand" is_natural p1 false;
    natural_test "p2 has natural hand" is_natural p2 true;
    natural_test "p3 does not have natural hand" is_natural p3 false;
    natural_test "d_with_hidden does not have natural hand"
      is_dealer_natural d_with_hidden false;
    has_ace_test "Player with only an ace" player_only_ace true;
    has_ace_test "Player with no cards" p0 false;
    has_ace_test "Player with ace and mutltiple cards" p4 true;
    has_ace_test "Played with no ace and multiple cards" p1 false;
    has_ace_test "Player with multiple aces" player_all_aces true;
    player_test "Player with only an ace"
      (ace_to_eleven player_only_ace)
      ("Bob Carlos", [ "Ace of Hearts" ], 11, false, 0, 0);
    player_test "Player with no aces" (ace_to_eleven p1)
      ( "Bob Carlos",
        [ "Five of Hearts"; "Queen of Spades" ],
        15,
        false,
        0,
        0 );
    player_test "Testing ace_to_eleven: Player with only an ace"
      (player_only_ace |> ace_to_eleven)
      ("Bob Carlos", [ "Ace of Hearts" ], 11, false, 0, 0);
    player_test
      "Testing ace_to_eleven: Player starts with an ace, applies at11 \
       and then another ace is added"
      (player_only_ace |> ace_to_eleven |> add_card ("Ace of Spades", 1))
      ( "Bob Carlos",
        [ "Ace of Hearts"; "Ace of Spades" ],
        22,
        true,
        0,
        0 );
    player_test "Testing ace_to_eleven: Player with natural hand"
      (p4 |> ace_to_eleven)
      ( "Bob Carlos",
        [ "Ace of Hearts"; "Ten of Spades" ],
        21,
        false,
        50,
        0 );
    player_test
      "Testing ace_to_eleven: Player with only ace, applied multiple \
       times"
      (player_only_ace |> ace_to_eleven |> ace_to_eleven)
      ("Bob Carlos", [ "Ace of Hearts" ], 11, false, 0, 0);
    player_test "Testing ace_to_eleven: Player has all aces"
      (player_all_aces |> ace_to_eleven)
      ( "Bob Carlos",
        [
          "Ace of Hearts";
          "Ace of Spades";
          "Ace of Clubs";
          "Ace of Diamonds";
        ],
        44,
        true,
        0,
        0 );
    has_pair_test "Player with a pair of twos" p_2pair true;
    has_pair_test "Player with a pair of kings" p_kingpair true;
    has_pair_test "Player does NOT have a pair" p3 false;
    has_pair_test "Player has two cards with same value" p8 false;
    player_switchhand_test "Player has a valid second hand" p_2split
      [ "Two of Hearts" ];
    player_switchhand_test
      "Player has 2 hands with different number of cards"
      (p_kingpair |> split_pair |> add_card ("Ace of Spades", 1))
      [ "King of Spades" ];
    player_hassndhand_test "Player has no second hand" p_2pair false;
    player_hassndhand_test "Player has a second hand" p_2split true;
    split_test "Player splits pair of twos" p_2pair
      ([ "Two of Diamonds" ], [ "Two of Hearts" ]);
    split_test "Player splits pair of kings" p_kingpair
      ([ "King of Hearts" ], [ "King of Spades" ]);
    player_hasdouble_test "player has valid double" p_double true;
    player_hasdouble_test
      "player has valid total value but invalid card number"
      (p_kingpair |> split_pair)
      false;
    player_hasdouble_test "player has 2 cards but invalid total value"
      p_kingpair false;
    player_insurance_test "positive insurance operation" 10 ( + ) p0 10;
    player_insurance_test "positive insurance operation" 6 ( - ) p0 (-6);
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
    parse_number_exception_test "Parse quit input in sentence"
      "quit please" Malformed;
    parse_number_exception_test "Parse empty input" "" Empty;
    parse_number_exception_test "Parse space only input" "    " Empty;
    parse_number_exception_test "Parse quit input" "quit" Escape;
    parse_number_exception_test "Parse help input" "help" Help;
    parse_name_test "Parse valid name: Bob" "  Bob  " [] [ "Bob" ];
    parse_name_test "Parse valid name : Henry Conlon" "Henry   Conlon"
      [ "Bob" ]
      [ "Bob"; "Henry Conlon" ];
    parse_name_exception_test "Parse empty name" "" [] Empty;
    parse_name_exception_test "Parse space only name" "    " [ "Bob" ]
      Empty;
    parse_name_exception_test "Parse name already in list" "Bob"
      [ "Henry"; "Bob" ] Malformed;
    parse_name_exception_test "Parse quit" "quit" [] Escape;
    parse_name_exception_test "Parse help" "help" [] Help;
    parse_command_test "Parse hit command" "  hit " Hit;
    parse_command_test "Parse stand command" "stand " Stand;
    parse_command_test "Parse stand command" "   split " Split;
    parse_command_test "Parse ace to eleven" "ace  to   eleven  "
      AceToEleven;
    parse_command_test "Parse play command" "  play" Play;
    parse_command_test "Parse double down command" "double down"
      DoubleDown;
    parse_command_test
      "Parse double down command, when extra space exists"
      "   double     down     " DoubleDown;
    parse_command_test "Parse play command" "  play" Play;
    parse_command_exception_test "Parse empty command" "" Empty;
    parse_command_exception_test "Parse space only command" "    " Empty;
    parse_command_exception_test "Parse invalid command for hit"
      "hit me" Malformed;
    parse_command_exception_test "Parse invalid command for stand"
      "stand up" Malformed;
    parse_command_exception_test "Parse invalid command for play"
      "play again" Malformed;
    parse_command_exception_test "Parse invalid command for split"
      "split pair" Malformed;
    parse_command_exception_test
      "Parse invalid command for bet with too many integers"
      "   bet 10 18" Malformed;
    parse_command_exception_test
      "Parse invalid bet command with a negative integer" "bet -10"
      Malformed;
    parse_command_exception_test
      "Parse invalid bet command with a float value" "bet 9.87"
      Malformed;
    parse_command_exception_test
      "Parse invalid bet command with a non-numeric input" "bet a pony"
      Malformed;
    parse_command_exception_test "Parse double down with illegal words"
      "double   e   down" Malformed;
    parse_command_exception_test "Parse invalid command for quit"
      "quit game now" Malformed;
    parse_command_exception_test
      "Parse invalid and unrecognized command" "open seasame" Malformed;
    parse_command_exception_test "Parse quit input in sentence"
      "quit please" Malformed;
    parse_command_exception_test "Parse quit" "quit" Escape;
    parse_command_exception_test "Parse help" "help" Help;
    parse_command_exception_test "Parse invalid help command" "help me"
      Malformed;
  ]

(* Sample states *)

(* A game with no naturals *)
let st0 = init_state 2 3 [ "Bob"; "Alice"; "Henry" ] [ 1; 2; 3 ]
let _ = print_endline "Initial state"
let _ = print_players (list_of_players st0)
let _ = print_dealer (st0 |> dealer_of |> show_hand)
let st1 = deal "Alice" st0 |> increase_bet 5 "Henry"
let _ = print_endline "Alice gets another card"
let _ = print_players (list_of_players st1)
let _ = print_dealer (st1 |> dealer_of |> show_hand)

let _ =
  List.map current_bet (players_of st1)
  |> List.map string_of_int |> String.concat ", " |> print_endline

let st2 = complete_hand st1
let _ = print_endline "Dealer completes his hand"
let _ = print_players (list_of_players st2)
let _ = print_dealer (st2 |> dealer_of |> show_hand)
let st3 = reset_all st2
let _ = print_endline "Reset hands of all players and the dealer"
let _ = print_players (list_of_players st3)
let _ = print_dealer (st3 |> dealer_of |> show_hand)

let _ =
  List.map current_total (players_of st3)
  |> List.map string_of_int |> String.concat ", " |> print_endline
(* A game where only dealer has natural *)

let st4 = 1

let state_tests =
  [
    init_state_test "test initial state" 2 3
      [ "Bob"; "Alice"; "Henry" ]
      [ 1; 2; 3 ];
    init_state_exception_test
      "test that init_state raises InvalidInput exception" 2 4
      [ "Bob"; "Alice"; "Henry" ]
      [ 1; 2; 3 ] InvalidInput;
    state_hiddencard_test "test dealing a card to a player" 2 st1;
    state_completedealer_test "test dealer completes hand" 2 st2;
    state_resetall_test "test hand of cards is reset for all players"
      st3;
    state_increasebet_test "test increase bet" 1 "Bob" st0
      ([ 1; 0; 0 ], [ 1; 2; 3 ]);
    state_redeembet_test "test rewarding chips for a winning player"
      ( + ) "Henry" st1
      ([ 0; 0; 0 ], [ 1; 2; 8 ]);
    state_redeembet_test "test taking chips for a losing player" ( - )
      "Henry" st1
      ([ 0; 0; 0 ], [ 1; 2; -2 ]);
    state_insurance_test
      "test increasing and redeeming insurance with addition" 10 ( + )
      "Bob" st0 [ 11; 2; 3 ];
  ]

let suite =
  "test suite for BlackJack"
  >::: List.flatten
         [ cards_tests; player_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
