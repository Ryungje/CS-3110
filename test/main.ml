open OUnit2
open Game

let cards_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ cards_tests]

let _ = run_test_tt_main suite