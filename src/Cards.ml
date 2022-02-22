open Yojson.Basic.Util

type card = {
  name : string;
  value : int;
}

type deck = card list * int

let card_of_json j =
  {
    name = j |> member "name" |> to_string;
    value = j |> member "value" |> to_int;
  }

let deck_of_json j = j |> to_list |> List.map card_of_json

let standard_deck =
  try deck_of_json (Yojson.Basic.from_file "data/standardDeck.json")
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let rec reset n =
  let rec make_deck n =
    match n with
    | 0 -> []
    | n -> standard_deck @ make_deck (n - 1)
  in
  (make_deck n, n)

let cmp_tup (x, _) (y, _) = Int.compare x y
let snd (_, y) = y

let shuffle d =
  let rec rng = function
    | [] -> []
    | h :: t -> Random.int 65536 :: rng t
  in
  List.combine (rng d) d
  |> List.sort cmp_tup |> List.split
  |>
  let snd (x, y) = y in
  snd

let peek d =
  match d with
  | dck, _ -> (
      match dck with
      | [] -> raise (Failure "deck is empty")
      | h :: _ -> (h.name, h.value))

let pop d =
  match d with
  | (dck : card list), (n : int) -> (
      match dck with
      | [] -> raise (Failure "deck is empty")
      | h :: t -> (t, n))
