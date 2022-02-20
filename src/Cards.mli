(** Representation of stack-like deck of cards.
    
    This module represents the data stored within the deck of cards and each 
    individual card. It handles alterations to the deck and querying of the 
    data. *)

type deck
(** The abstract type of values representing a stack of cards to be used in the game. *)

val reset : int -> deck 
(** [reset n] is the stack of cards created from [n] number of full standard deck of cards. Requires: [n] >= 1 *)