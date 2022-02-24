(** Representation of dynamic state of the game BlackJack.

    This module represents the state of the BlackJack game, including
    the current deck of cards, the dealer, and each player. *)

open Cards
open Player

exception InvalidInput
(** Raised when inputs for game are not valid*)

type s
(** The abstract type of values representing the game state. *)

val init_state : int -> int -> string list -> s
(** [init_state num_deck num_player player_names] is the initial state
    of the game when playing Blackjack with [num_deck] amount of
    standard card decks, [num_player] amount of players (not including
    the dealer), and [player_names] as the ordered list of names
    corresponding to player 1, player 2, and etc. Each player should be
    dealt 2 cards. The dealer should be dealt one shown card and one
    hidden card.

    Raises: [InvalidInput] if length of the list [player_names] is not
    equal to[num_player] *)

val deal : string -> s -> s
(** [deal pname st] is the state of the game after player with the name
    [pname] wants to hit and receives another card. Requires: [pname] is
    the name of a valid player part of a valid state [st]. *)

val complete_hand : s -> s
(** [complete_hand st] is the state of the game [st] after the dealer
    completes his hand (i.e. the dealer keeps taking cards until the
    total is 17 or higher. Requires: [st] is a valid state. *)

val list_of_players : s -> (string * string list) list
(** [list_of_players st] is a list containing the name and list of cards
    in each player's hand from [st]. Requires: [st] is a valid state. *)

val dealer_of : s -> player
(** [hand_of_dealer st] is the list of cards in the dealer's hand from
    [st]. Requires: [st] is a valid state. *)

val remaining_deck : s -> deck
(** [remaining_deck st] is the list of remaining cards in the deck.
    Requires: [st] is a valid state. *)
