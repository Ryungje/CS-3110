(** Representation of a player (dealer included).

    A valid player can have a name, hold a hand of cards, bet, and
    collect rewards. A dealer is a special type of valid player that can
    have a hidden card but cannot bet.

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

val current_bet : player -> int
(** [current_bet p] is the total amount of bet that [p] placed in its
    current hand of cards. *)

val current_total : player -> int
(** [current_total p] is the total amount that [p] has in pocket. *)

val is_bust : player -> bool
(** [is_bust p] is whether the player's hand busted (i.e. the value of
    the player's hand is greater than 21). Requires: [p] must be a valid
    player. *)

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
    shown hand. Requires: [d] must be a valid dealer. *)

val add_bet : int -> player -> player
(** [add_bet amount p] is the player after [p] increases its bet by
    [amount]. Requires: [amount] > 0. *)

val redeem : (int -> int -> int) -> player -> player
(** [redeem operator p] is the player after [p] adds its bet to its
    total according to [operator] and resets its bet to nothing.
    Requires: [operator] should be (+) or (-). *)

val is_natural : player -> bool
(** [is_natural p] is whether or not the [p] has a natural (i.e. the
    player's first two cards is an ace and a ten-card). Requires: [p] is
    a valid player who only has two cards. *)

val redeem_for_natural : bool -> player -> player
(** [redeem_for_natural b p] is the player [p] who collects 1.5 times
    their starting bet if [b]. Otherwise, the player simply resets its
    bet back to zero. Requires: [p] is a valid player. *)

val is_dealer_natural : player -> bool
(** [is_dealer_natural d] is whether or not the [d] has a natural (i.e.
    the the face-up card and hidden card makes a pair of ace and
    ten-card). Requires: [d] is a valid dealer who only has one face-up
    card and one hidden card. *)

val has_ace : player -> bool
(** [has_ace p] is whether or not [p] is currently holding an ace card.
    Requires: [p] is a valid player. *)

val ace_to_eleven : player -> player
(** [ace_to_eleven p] is the player when the value of every ace card in
    the hand of [p] is changed to eleven. Requires: [p] is a valid
    player. *)

val has_pair : player -> bool
(** [has_pair] is whether or not [p] is currently holding a pair of
    cards with the same denomination. For example, two jacks or two
    sixes are a pair, but a jack and a queen is not a pair. Requires:
    [p] is only holding two cards *)

val split_pair : player -> player
(** [split_pair p] represents a player that splits its initial two cards
    into two separate hands such that [p] is currently holding one card
    in one hand as the current hand for play and holding the other card
    on the side as separate hand to play. Requires: [p] must has have a
    pair of cards with same denominations. *)

val switch_hands : player -> player
(** [switch_hands p] represents a player that set its hand aside and is
    now holding its other set of cards that was originally set aside.
    Requires: [p] must has have a non-empty second hand of cards. *)

val has_snd_hand : player -> bool
(** [has_snd_hand p] is whether or not [p] splitted a pair of cards and
    has two hands on the table. Requires: [p] must be a valid player. *)

val has_double : player -> bool
(** [has_doubled p] is whether or not [p] can doubled down (i.e. when
    the initial two cards totals to 9, 10, or 11). Requires: [p] must be
    a valid player*)

val double_bet : player -> player
(** [double_bet p] is player that doubles their bet when the intial two
    cards deal to the player [p] totals to 9, 10, or 11. If player does
    not have exactly two cards or the card value does not total to 9, 10
    or 11, then nothing changes. Requires: [p] must be a valid player.*)

val add_insurance : int -> player -> player
(** [add_insurnace amount p] is the player after [p] increases its
    insurance by [amount]. Requires: [amount] >= 0. *)

val redeem_for_insurance : (int -> int -> int) -> player -> player
(** [redeem_for_insurance operator p] is the player after [p] adds its
    insurance to its total according to [operator] and resets its
    insurance to nothing. Requires: [operator] should be be using the
    total of [p] as the first input and [amount] as the second input. *)
