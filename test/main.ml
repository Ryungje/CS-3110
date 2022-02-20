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
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(******************************************************************* Add
  unit tests for modules below.
  ********************************************************************)

let cards_tests = []
let dealer_tests = []
let player_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ cards_tests; dealer_tests; player_tests ]

let _ = run_test_tt_main suite