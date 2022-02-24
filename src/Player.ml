exception Empty

type player = {
  name : string;
  hand : string list;
  value : int;
  bet : int;
  total : int;
  hidden_card : string * int;
}

let init_stats str =
  if str = "" then raise Empty
  else
    {
      name = str;
      hand = [];
      value = 0;
      bet = 0;
      total = 0;
      hidden_card = ("", 0);
    }

let name_of p = p.name

let add_card c p =
  match c with
  | n, v -> { p with hand = p.hand @ [ n ]; value = p.value + v }

let show_hand p = p.hand
let hand_value p = p.value
let reset_hand p = { p with hand = []; value = 0 }
let add_hidden c p = { p with hidden_card = c }

let reveal p =
  let new_p = add_card p.hidden_card p in
  { new_p with hidden_card = ("", 0) }
