open Cards
open Player

exception InvalidInput

type s = {
  players : player list;
  dealer : player;
  card_deck : deck;
}

let players_of st = st.players
let dealer_of st = st.dealer
let remaining_deck st = st.card_deck

let rec deal_to_all_players player_list deck acc =
  match player_list with
  | [] -> (acc, deck)
  | h :: t ->
      deal_to_all_players t (pop deck) (acc @ [ add_card (peek deck) h ])

let rec make_player_list n names acc =
  match names with
  | [] -> acc
  | h :: t -> make_player_list (n - 1) t (acc @ [ init_stats h ])

let init_state num_deck num_player player_names bet_list =
  if num_player <> List.length player_names then raise InvalidInput
  else
    let player_list_no_bets =
      make_player_list num_player player_names []
    in
    let player_list_with_bets =
      List.map2 add_bet bet_list player_list_no_bets
    in
    let player_list = List.map (redeem ( + )) player_list_with_bets in
    let new_dealer = init_stats "Dealer" in
    let curr_deck = reset num_deck |> shuffle in
    let dealer_with_card = add_card (peek curr_deck) new_dealer in
    let players_with_1card, new_deck =
      deal_to_all_players player_list (pop curr_deck) []
    in
    let dealer_with_hidden =
      add_hidden (peek new_deck) dealer_with_card
    in
    let players_with_2cards, end_deck =
      deal_to_all_players players_with_1card (pop new_deck) []
    in
    {
      players = players_with_2cards;
      dealer = dealer_with_hidden;
      card_deck = end_deck;
    }

let rec add_card_to c pname p_list acc =
  match p_list with
  | [] -> acc
  | h :: t ->
      if name_of h = pname then acc @ [ add_card c h ] @ t
      else add_card_to c pname t (acc @ [ h ])

let deal pname st =
  let updated_player_list =
    add_card_to (peek st.card_deck) pname st.players []
  in
  {
    st with
    players = updated_player_list;
    card_deck = pop st.card_deck;
  }

let rec add_bet_to amount pname p_list acc =
  match p_list with
  | [] -> acc
  | h :: t ->
      if name_of h = pname then acc @ [ add_bet amount h ] @ t
      else add_bet_to amount pname t (acc @ [ h ])

let increase_bet amount pname st =
  let updated_player_list = add_bet_to amount pname st.players [] in
  { st with players = updated_player_list }

let rec redeem_for operator pname p_list acc =
  match p_list with
  | [] -> acc
  | h :: t ->
      if name_of h = pname then acc @ [ redeem operator h ] @ t
      else redeem_for operator pname t (acc @ [ h ])

let redeem_bet operator pname st =
  let updated_player_list = redeem_for operator pname st.players [] in
  { st with players = updated_player_list }

let rec adding_cards p d =
  if hand_value p < 17 then adding_cards (add_card (peek d) p) (pop d)
  else (p, d)

let complete_hand st =
  let complete_dealer, new_deck =
    adding_cards (reveal st.dealer) st.card_deck
  in
  { st with dealer = complete_dealer; card_deck = new_deck }

let reset_all st =
  let reset_players = List.map (fun p -> reset_hand p) st.players in
  let reset_dealer = reset_hand st.dealer in
  let curr_deck = remaining_deck st in
  let dealer_with_card = add_card (peek curr_deck) reset_dealer in
  let players_with_1card, new_deck =
    deal_to_all_players reset_players (pop curr_deck) []
  in
  let dealer_with_hidden =
    add_hidden (peek new_deck) dealer_with_card
  in
  let players_with_2cards, end_deck =
    deal_to_all_players players_with_1card (pop new_deck) []
  in
  {
    players = players_with_2cards;
    dealer = dealer_with_hidden;
    card_deck = end_deck;
  }

let list_of_players st =
  let rec get_players p_list acc =
    match p_list with
    | [] -> acc
    | h :: t -> get_players t (acc @ [ (name_of h, show_hand h) ])
  in
  get_players st.players []
