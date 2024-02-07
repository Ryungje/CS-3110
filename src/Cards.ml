open Yojson.Basic.Util

type card = {
  name : string;
  value : int;
}

type deck = {
  cards : card list;
  n : int;
}

let card_of_json j =
  {
    name = j |> member "name" |> to_string;
    value = j |> member "value" |> to_int;
  }

let deck_of_json j = j |> to_list |> List.map card_of_json

let standard_deck =
  try deck_of_json (Yojson.Basic.from_file "data/standardDeck.json")
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let rec reset num =
  let rec make_deck num =
    match num with
    | 0 -> []
    | n -> standard_deck @ make_deck (n - 1)
  in
  { cards = make_deck num; n = num }

let _ = Random.init (int_of_float (Unix.gettimeofday ()))

let shuffle d =
  let paired_list =
    List.map (fun x -> (Random.float 2. ** 30., x)) d.cards
  in
  let shuffled_paired_list = List.sort compare paired_list in
  let shuffled_list =
    List.map
      (fun x ->
        match x with
        | _, c -> c)
      shuffled_paired_list
  in
  { d with cards = shuffled_list }

let peek d =
  match d.cards with
  | [] -> raise (Failure "deck is empty")
  | h :: _ -> (h.name, h.value)

let pop d =
  match d.cards with
  | [] -> raise (Failure "deck is empty")
  | h :: t ->
      if t = [] then reset d.n |> shuffle else { d with cards = t }

let cards_of d = d.cards

(* Helper functions for card_display *)
let get_suit str =
  match String.split_on_char ' ' str with
  | _ :: _ :: [ "Spades" ] -> "♠"
  | _ :: _ :: [ "Hearts" ] -> "♥"
  | _ :: _ :: [ "Clubs" ] -> "♣"
  | _ :: _ :: [ "Diamonds" ] -> "♦"
  | _ -> ""

let get_number str =
  match String.split_on_char ' ' str with
  | "Ace" :: _ -> "A"
  | "Two" :: _ -> "2"
  | "Three" :: _ -> "3"
  | "Four" :: _ -> "4"
  | "Five" :: _ -> "5"
  | "Six" :: _ -> "6"
  | "Seven" :: _ -> "7"
  | "Eight" :: _ -> "8"
  | "Nine" :: _ -> "9"
  | "Ten" :: _ -> "10"
  | "Jack" :: _ -> "J"
  | "Queen" :: _ -> "Q"
  | "King" :: _ -> "K"
  | _ -> ""
(* looks at string input and looks for suit and returns corresponding
   symbol *)

let rec build_tops (hand : string list) =
  if List.length hand > 0 then
    "┌───────┐   " ^ build_tops (List.tl hand)
  else ""

let rec build_bottoms (hand : string list) =
  if List.length hand > 0 then
    "└───────┘   " ^ build_bottoms (List.tl hand)
  else ""

let rec build_middles (hand : string list) =
  if List.length hand > 0 then
    "│       │   " ^ build_middles (List.tl hand)
  else ""

let rec build_numbers (hand : string list) top =
  if List.length hand > 0 then
    let number = List.hd hand |> get_number in
    let space = if String.length number = 2 then "" else " " in
    if top then
      "│" ^ number ^ space ^ "     │   "
      ^ build_numbers (List.tl hand) top
    else
      "│     " ^ space ^ number ^ "│   "
      ^ build_numbers (List.tl hand) top
  else ""

let rec build_suits (hand : string list) =
  if List.length hand > 0 then
    let suit = List.hd hand |> get_suit in
    "│   " ^ suit ^ "   │   " ^ build_suits (List.tl hand)
  else ""

let rec card_display_helper hand =
  print_endline (build_tops hand);
  print_endline (build_numbers hand true);
  print_endline (build_middles hand);
  print_endline (build_suits hand);
  print_endline (build_middles hand);
  print_endline (build_numbers hand false);
  print_endline (build_bottoms hand)

let card_display hand = card_display_helper hand
