(** Representation of dynamic state of the game BlackJack.

    This module represents the state of the BlackJack game, including the current deck of cards, the dealer, and each player. *)

exception InvalidInput
(** Raised when inputs for game are not valid*)

type s
(** The abstract type of values representing the game state. *)

val init_state : int -> int -> string list -> s
(** [init_state num_deck num_player player_names] is the initial state of the  game when playing Blackjack with [num_deck] amount of standard card decks, [num_player] amount of players (not including the dealer), and [player_names] as the ordered list of names corresponding to player 1, player 2, and etc. Each player should be dealt 2 cards. The dealer should be dealt one shown card and one hidden card.
  
  Raises: [InvalidInput] if length of the list [player_names] is not equal to[num_player] *) 

val deal : player -> s -> s
(** [deal p st] is the state of the game after player [p] wants to hit and receives another card. Requires: [p] is a valid player part of a valid state [st]. *) 

val complete_hand : s -> s
(** [complete_hand st] is the state of the game [st] after the dealer completes his hand (i.e. the dealer keeps taking cards until the total is 17 or higher. Requires: [st] is a valid state. *)