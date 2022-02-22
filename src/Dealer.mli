(** Representation of a dealer, a special type of AI player.

    This module represents the data stored within the dealer of the
    game. It handles alterations to the data and querying of the data. *)

open Cards
include Player

val reveal : bool
(** [reveal] is whether or not the dealer is ready to reveal his hand. *)

val reveal_hand : reveal -> player -> string list 
(** [reveal_hand r p] is the list of cards shown by the dealer [p] to the other players. If [r] is true, show entire hand of cards. If [r] is false, show only the first card. Requires: [p] must be a valid player. *)

val complete_hand : deck -> player -> deck
(** [complete_hand d p] is the remaining deck of cards after the dealer [p]completes his hand with deck [d]. A hand is complete when the value of the hand is 17 or more. Requires: [p] must be a valid player and [d] must be a valid deck. *)