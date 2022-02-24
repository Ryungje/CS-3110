open OUnit2
open Game
open Cards
open Commands
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

(**********************************************************************
  Add unit tests for modules below.
  ********************************************************************)

let card_deck = reset 2
let one_card_deck = repeat pop 51 (reset 1)

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

let dealer_tests = []
let player_tests = []

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

let state_tests = []

let suite =
  "test suite for BlackJack"
  >::: List.flatten
         [
           cards_tests;
           dealer_tests;
           player_tests;
           command_tests;
           state_tests;
         ]

let _ = run_test_tt_main suite