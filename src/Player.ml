exception Empty

type player = {
  name : string;
  hand : string list;
  value : int;
  bet : int;
  total : int;
  hidden_card : string * int;
  ace_is_eleven : bool;
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
      ace_is_eleven = false;
    }

let name_of p = p.name

let add_card c p =
  match c with
  | n, v -> { p with hand = p.hand @ [ n ]; value = p.value + v }

let show_hand p = p.hand
let hand_value p = p.value
let current_bet p = p.bet
let current_total p = p.total
let is_bust p = p.value > 21

let reset_hand p =
  { p with hand = []; value = 0; bet = 0; ace_is_eleven = false }

let add_hidden c p = { p with hidden_card = c }

let reveal p =
  let new_p = add_card p.hidden_card p in
  { new_p with hidden_card = ("", 0) }

let add_bet amount p = { p with bet = p.bet + amount }

let redeem operator p =
  { p with bet = 0; total = operator p.total p.bet }

let is_natural p =
  if List.filter (fun str -> String.sub str 0 3 = "Ace") p.hand = []
  then false
  else if
    List.filter
      (fun str ->
        String.sub str 0 3 = "Ten"
        || String.sub str 0 4 = "Jack"
        || String.sub str 0 5 = "Queen"
        || String.sub str 0 4 = "King")
      p.hand
    |> List.length <> 1
  then false
  else true

let redeem_for_natural b p =
  if b then { p with bet = 0; total = p.total + p.bet + (p.bet / 2) }
  else { p with bet = 0 }

let is_dealer_natural p =
  let temp_card_lst = p.hand @ [ fst p.hidden_card ] in
  is_natural { p with hand = temp_card_lst }

let has_ace p =
  List.filter (fun str -> String.sub str 0 3 = "Ace") p.hand <> []

let ace_to_eleven p =
  if p.ace_is_eleven then p
  else
    let sum =
      List.fold_left
        (fun acc c ->
          if String.sub c 0 3 = "Ace" then acc + 10 else acc)
        0 p.hand
    in
    { p with value = p.value + sum; ace_is_eleven = true }
