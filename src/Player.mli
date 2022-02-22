(** Representation of a player.

    This module represents the data stored within each player
    participating in the game. It handles alterations to the data and
    querying of the data. *)

exception Empty
(** Raised when input is empty *)

type player
(** The abstract type of values repesenting a player participating in the game, such as name, current hand, value of current hand, current bet, and current total balance to be paid or collected. *)

val init_stats : string -> player
(** [init_stats str] is the initial stats of a player at the start of the game. The player should have name [str], no cards in hand, no bets, and no collected rewards. Note: a normal player cannot have a hidden card or value.
    
    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces. *)

val add_card : string * int -> player -> player
(** [add_card c p] is the player [p] who received another card [c] to
    add to their hand. Requires: [c] must be a valid card from type deck
    and [p] must be a valid player *)

val show_hand : player -> string list
(** [show_hand p] is the list of cards currently in the hands of player
    [p]. Requires: [p] must be a valid player. *)

val hand_value : player -> int
(** [hand_value p] is the value of the cards currently in the hands of
    player [p]. Requires: [p] must be a valid player. *)

val reset_hand : player -> player
(** [reset_hand p] is the player with an empty hand. Requires: [p] must be a valid player. *)