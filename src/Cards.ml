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

let card_display symbol =
  print_endline "┌─────────┐";
  print_endline "│{}{}       │";
  print_endline "│         │";
  print_endline "│         │";
  print_endline "│    {}    │";
  print_endline "│         │";
  print_endline "│         │";
  print_endline "│       {}{}│";
  print_endline "└─────────┘"
