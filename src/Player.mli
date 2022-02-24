(** Representation of a player.

    Note: A normal player can have a name, hold a hand of cards, bet,
    and collect rewards. A dealer is a special type of player that
    cannot bet but can have a hidden card.

    This module represents the data stored within each player
    participating in the game. It handles alterations to the data and
    querying of the data. *)

exception Empty
(** Raised when input is empty *)

type player
(** The abstract type of values repesenting a player participating in
    the game, such as name, current hand, value of current hand, current
    bet, and current total balance to be paid or collected. *)

val init_stats : string -> player
(** [init_stats str] is the initial stats of a player at the start of
    the game. The player should have name [str], no cards in hand, no
    bets, and no collected rewards.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces. *)

val name_of : player -> string
(** [name_of p] is the name of player [p]. Requires: [p] is a valid
    player. *)

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
(** [reset_hand p] is the player with an empty hand. Requires: [p] must
    be a valid player. *)

val add_hidden : string * int -> player -> player
(** [add_hidden c d] is the dealer [d] who received a hidden card [c] to
    add to their hand. This hidden card should not be part of the shown
    hand or shown value until it is revealed. Requires: [c] is a valid
    card and [d] is a valid dealer. *)

val reveal : player -> player
(** [reveal d] is the dealer [d] who added their hidden card to their
    shown hand. Requires: d] must be a valid dealer. *)