(** Representation of dynamic state of the game BlackJack.

    This module represents the state of the BlackJack game, including
    the current deck of cards, the dealer, and each player. *)

open Cards
open Player

exception InvalidInput
(** Raised when inputs for game are not valid*)

type s
(** The abstract type of values representing the game state. *)

val init_state : int -> int -> string list -> int list -> s
(** [init_state num_deck num_player player_names bet_list] is the
    initial state of the game when playing Blackjack with [num_deck]
    amount of standard card decks, [num_player] amount of players (not
    including the dealer), [player_names] and [bet_list] as the ordered
    list of names and total starting chips corresponding to player 1,
    player 2, and etc. Each player should be dealt 2 cards. The dealer
    should be dealt one shown card and one hidden card.

    Raises: [InvalidInput] if length of the list [player_names] is not
    equal to[num_player] *)

val make_empty_state : int -> s
(** [make_empty_state plist d deck] is the state constructed from the
    valid given [plist]: player list, [d]: dealer, and [deck]: deck of
    cards. *)

val players_of : s -> player list
(** [player_of st] is the list of players from [st]. Requires: [st] is a
    valid state. *)

val dealer_of : s -> player
(** [dealer_of st] is the list of cards in the dealer's hand from [st].
    Requires: [st] is a valid state. *)

val remaining_deck : s -> deck
(** [remaining_deck st] is the list of remaining cards in the deck.
    Requires: [st] is a valid state. *)

val deal : string -> s -> s
(** [deal pname st] is the state of the game after player with the name
    [pname] wants to hit and receives another card. Requires: [pname] is
    the name of a valid player part of a valid state [st]. *)

val increase_bet : int -> string -> s -> s
(** [increase_bet amount pname st] is the state of the game after player
    with the name [pname] increases bet by [amount]. All other
    attributes of [st] should remain unchanged. Requires: [pname] is the
    name of a valid player part of a valid state [st]. *)

val redeem_bet : (int -> int -> int) -> string -> s -> s
(** [increase_bet operator pname st] is the state of the game after
    player with the name [pname] redeems bet according to [operator] in
    [st]. This player should reset its bet back to zero and change its
    total according to [operator]. All other attributes of [st] should
    remain unchanged. Requires: [pname] is the name of a valid player
    part of a valid state [st]. *)

val unnatural_dealer_natural_player : s -> s
(** [unnatural_dealer_natural_player st] is the state of the game [st]
    after all natural players collect 1.5 times their bets and unnatural
    players simply resets their bet back to zero. Only bet and total
    attributes of players in [st] changes. Requires: dealer in [st] does
    not have a natural and at least one player in [st] has a natural. *)

val natural_dealer_unnatural_player : s -> s
(** [unnatural_dealer_natural_player st] is the state of the game [st]
    after where players with a natural simply reset their bet back to
    zero and players without a natural must pay their bet to the dealer.
    Requires: dealer in [st] has a natural. *)

val change_ace : string -> s -> s
(** [change_ace pname st] is the state of the game [st] when player with
    [pname] changes the value of their ace cards from one to eleven.
    Requires: [pname] must the name of a player in [st]. *)

val split_hand : string -> s -> s
(** [split_hand pname st] is the state of the game [st] when player with
    [pname] splits their pair of cards with same denomination into two
    hands. Requires: [pname] must the name of a player in [st] with a
    valid pair of cards. *)

val swap_hand : string -> s -> s
(** [swap_hand pname st] is the state of the game [st] when player with
    [pname] swaps their current hand for the other set of hand left on
    the side when they split their cards. Requires: [pname] must the
    name of a player in [st] that splitted their cards in the current
    round of blackjack. *)

val complete_hand : s -> s
(** [complete_hand st] is the state of the game [st] after the dealer
    completes his hand (i.e. the dealer keeps taking cards until the
    total is 17 or higher. Requires: [st] is a valid state. *)

val reset_all : s -> s
(** [reset_all st] is the state of the game [st] after all the players
    and the dealer reset their hands and each receive 2 new cards, but
    all names, bets, and collected rewards remain the same. *)

val list_of_players : s -> (string * string list) list
(** [list_of_players st] is a list containing the name and list of cards
    in each player's hand from [st]. Requires: [st] is a valid state. *)
