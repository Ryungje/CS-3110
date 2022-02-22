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
let shuffle_test
    (name : string)
    (d : 'a list)
    (expected_output : 'a list) : test =
  name >:: fun _ -> assert_equal true (cmp_lists d expected_output)

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
    (expected_output : Cards.deck) : test =
  name >:: fun _ -> assert_equal expected_output (pop d)

(******************************************************************* Add
  unit tests for modules below.
  ********************************************************************)

let cards_tests = [ peek_test ]
let dealer_tests = []
let player_tests = []
let command_tests = []
let state_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           cards_tests;
           dealer_tests;
           player_tests;
           command_tests;
           state_tests;
         ]

let _ = run_test_tt_main suite