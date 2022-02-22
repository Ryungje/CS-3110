open Cards
open Player
open Dealer

type s = {
  players : player list;
  dealer : player;
  card_deck : deck 
}

let init_state num_deck num_player player_names = raise (Failure "Not Implemented")
let deal p st = raise (Failure "Not Implemented")
let complete_hand st = raise (Failure "Not Implemented")

