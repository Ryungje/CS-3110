(** Parsing of player inputs*)

(** The type [command] represents a player command. *)
type command =
  | Hit
  | Stand
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is encountered. *)

val parse_number : string -> int
(** [parse_number i] parses a player's input number [i] into an int
    value to be used in instantiating the number of standards decks or
    instantiating the number of partcipating players in the game.

    Raises: [Empty] if [i] is an empty string or contains only spaces.

    Raises: [Malformed] if [i] is cannot be trimmed to a signle numeric
    (0-9) string without spaces or if [parse_number i] is <=0. *)

val parse_name : string -> string list -> string list
(** [parse_name n n_list] is the list containing all existing player
    names [n_list] in the system and the newly added player name [n].
    [n] should remove extra spaces before being added to end of
    [n_list]. Requires: [n] contains only alphanumeric (A-Z, a-z, 0-9)
    and space characters (only ASCII character code 32; not tabs or
    newlines, etc.).

    Raises: [Empty] if [i] is an empty string or contains only spaces. *)

val parse_command : string -> command
(** [parse_command str] parses a player's input into a [command] as
    follows. [str] should only contain one word (i.e., consecutive
    sequence of non-sace caharacters) that is a verb that corresponds to
    a command.

    - [parse_command "   hit  "] is [Hit]
    - [parse_command "stand"] is [Stand]
    - [parse_command "play"] is [Play]
    - [parse_command "quit"] is [Quit]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [i] is an empty string or contains only spaces.

    Raises [Malformed] is command is malformed. A command is malformed
    if the verb is not "hit","stand", "play", or "quit", or if the verb
    is followed by more words. *)
