open OUnit2
open Game
open Cards

(******************************************************************* Add
  definitions and helper functions for test cases below.
  ********************************************************************)

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
  assert_equal true (cmp_lists (cards d) (cards expected_output))

(** [peek_test name d expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [peek d]. *)
let peek_test
    (name : string)
    (d : Cards.deck)
    (expected_output : string * int) : test =
  name >:: fun _ -> assert_equal expected_output (peek d)

(** [pop_test name d expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [pop d]. *)
let pop_test
    (name : string)
    (d : Cards.deck)
    (expected_output : string * int) : test =
  name >:: fun _ -> assert_equal expected_output (pop d |> peek)

(******************************************************************* Add
  unit tests for modules below.
  ********************************************************************)

let card_deck = reset 2

let cards_tests =
  [
    peek_test "Testing peek" card_deck ("Ace of Clubs", 1);
    pop_test "Testing pop" card_deck ("Two of Clubs", 2);
    shuffle_test "Testing shuffle" card_deck card_deck;
  ]

let dealer_tests = []
let player_tests = []
let command_tests = []
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