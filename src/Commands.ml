type command = Hit | Stand | Quit

exception Empty
exception Malformed 

let parse_number i = raise (Failure "Not Implemented")
let parse_name n n_list = raise (Failure "Not Implemented")
let parse_command str = raise (Failure "Not Implemented")