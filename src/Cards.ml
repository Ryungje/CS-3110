open Yojson.Basic.Util

type card = {
  name : string;
  value : int;
}

type deck = card list

let card_of_json j =
  {
    name = j |> member "name" |> to_string;
    value = j |> member "value" |> to_int;
  }

let deck_of_json j = j |> to_list |> List.map card_of_json

let standard_deck =
  try deck_of_json (Yojson.Basic.from_file "data/standardDeck.json")
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let rec reset = function
  | 0 -> []
  | n -> standard_deck @ reset (n - 1)

let shuffle d = raise (Failure "Not Implemented")

let peek d = function
  | [] -> raise (Failure "deck is empty")
  | h :: _ -> (h.name, h.value)

let pop d = raise (Failure "Not Implemented")
