(** Parsing of player inputs*)

(** The type [command] represents a player command. *)
type command =
  | Hit
  | Stand
  | AceToEleven
  | Split
  | DoubleDown
  | Play

exception Escape
(** Raised when user inputs command for quitting the game. *)

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is encountered. *)

val parse_number : string -> int
(** [parse_number i] parses a player's input number [i] into an int
    value to be used in instantiating the number of standards decks or
    instantiating the number of partcipating players in the game.

    Requires: [n] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [i] is an empty string or contains only spaces.

    Raises: [Malformed] if [i] cannot be trimmed to a single numeric
    (0-9) string without spaces or if [parse_number i] is <=0.

    Raises: [Escape] if [i] is "quit". *)

val parse_name : string -> string list -> string list
(** [parse_name n n_list] is the list containing all existing player
    names [n_list] in the system and the newly added player name [n].
    [n] should remove extra spaces before being added to end of
    [n_list]. Requires: [n] contains only alphanumeric (A-Z, a-z, 0-9)
    and space characters (only ASCII character code 32; not tabs or
    newlines, etc.).

    Raises: [Empty] if [i] is an empty string or contains only spaces.

    Raises: [Malformed] if parsed [n] already exits in [n_list]

    Raises: [Escape] if [i] is "quit". *)

val parse_command : string -> command
(** [parse_command str] parses a player's input into a [command] as
    follows. [str] should only contain one word (i.e., consecutive
    sequence of non-sace caharacters) that is a verb that corresponds to
    a command.

    - [parse_command "   hit  "] is [Hit]
    - [parse_command "stand"] is [Stand]
    - [parse_command "split"] is [Split]
    - [parse_command "    ace    to   eleven"] is [AceToEleven]
    - [parse_command "play"] is [Play]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [i] is an empty string or contains only spaces.

    Raises [Malformed] is command is malformed. A command is malformed
    if the verb is not "hit","stand", "split" or "play", or if the verb
    is followed by more words, or if the phrase "ace to eleven" contains
    extra characters that are not spaces.

    Raises: [Escape] if the verb is "quit". *)
