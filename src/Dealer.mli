(** Representation of a dealer, a special type of player who cannot bet but can have a hidden card.

    This module represents the data stored within the dealer of the
    game. It handles alterations to the data and querying of the data. *)

include Player

val add_hidden : string * int -> Player.player -> player
(** [add_hidden c p] is the dealer [p] who received a hidden card [c] to add to their hand. This hidden card should not be part of the shown hand or 
    shown value until it is revealed. Requires: [c] is a valid card and [p] is 
    a valid player. *)

val reveal :  Player.player -> player
(** [reveal p] is the dealer [p] who added their hidden card to their shown hand. Requires: [p] must be a valid player. *)
