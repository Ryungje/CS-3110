exception Empty

type player = {
  name : string;
  hand : string list;
  value : int;
  bet : int;
  total : int;
  hidden_card : string;
  hidden_value : int;
}

let init_stats str = raise (Failure "Not Implemented")
let name_of p = raise (Failure "Not Impelmented")
let add_card c p = raise (Failure "Not Implemented")
let show_hand p = raise (Failure "Not Implemented")
let hand_value p = raise (Failure "Not Implemented")
let reset_hand p = raise (Failure "Not Implemented")