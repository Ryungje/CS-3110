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

let cmp_tup (x, _) (y, _) = Int.compare x y
let snd (_, y) = y

let shuffle d =
  let cs =
    let rec rng = function
      | [] -> []
      | h :: t -> Random.int 65536 :: rng t
    in
    List.combine (rng d.cards) d.cards
    |> List.sort cmp_tup |> List.split
    |>
    let snd (x, y) = y in
    snd
  in
  { d with cards = cs }

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