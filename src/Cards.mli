(** Representation of stack-like deck of cards.

    This module represents the data stored within the deck of cards and
    each individual card. It handles alterations to the deck and
    querying of the data. *)

type card
(** The abstract type of values representing a card to be used in the
    game. *)

type deck
(** The abstract type of values representing a stack of cards to be used
    in the game. Requires: [deck] can not be empty. *)

val reset : int -> deck
(** [reset n] is the new and complete stack of cards created from [n]
    number of full standard decks of cards. Requires: [n] >= 1 *)

val shuffle : deck -> deck
(** [shuffle d] is a randomly ordered stack of cards created from [d].
    Requires: [deck] is a valid deck. *)

val peek : deck -> string * int
(** [peek d] is the name and value of the card at the top of the stack
    of cards [d]. Requires: [d] is valid deck. *)

val pop : deck -> deck
(** [pop d] is the stack of cards [d] with the top card removed. If
    [pop d] is an empty stack after the top card is removed, then let
    [pop d] be a new, complete, and shuffled stack of cards. Requires:
    [d] is a valid deck. *)

val cards_of : deck -> card list
(** [cards_of d] is the stack of cards in deck. Requires: [d] is a valid
    deck. *)

val card_display : string list -> unit