exception Empty

type player = {
  name : string;
  hand : string list;
  value : int;
  snd_hand : string list * int;
  bet : int;
  total : int;
  hidden_card : string * int;
  ace_is_eleven : bool;
  doubled : bool;
}

let init_stats str =
  if str = "" then raise Empty
  else
    {
      name = str;
      hand = [];
      value = 0;
      snd_hand = ([], 0);
      bet = 0;
      total = 0;
      hidden_card = ("", 0);
      ace_is_eleven = false;
      doubled = false;
    }

let name_of p = p.name

let add_card c p =
  if p.ace_is_eleven && String.sub (fst c) 0 3 = "Ace" then
    { p with hand = p.hand @ [ fst c ]; value = p.value + snd c + 10 }
  else
    match c with
    | n, v -> { p with hand = p.hand @ [ n ]; value = p.value + v }

let show_hand p = p.hand
let hand_value p = p.value
let current_bet p = p.bet
let current_total p = p.total
let is_bust p = p.value > 21

let reset_hand p =
  {
    p with
    hand = [];
    value = 0;
    snd_hand = ([], 0);
    bet = 0;
    ace_is_eleven = false;
    doubled = false;
  }

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

let convert_cname_to_cval str =
  match String.sub str 0 3 with
  | "Ace" -> 1
  | "Two" -> 2
  | "Thr" -> 3
  | "Fou" -> 4
  | "Fiv" -> 5
  | "Six" -> 6
  | "Sev" -> 7
  | "Eig" -> 8
  | "Nin" -> 9
  | "Ten" -> 10
  | "Jac" -> 10
  | "Que" -> 10
  | "Kin" -> 10
  | _ -> failwith "Not a viable card"

let has_pair p =
  match p.hand with
  | [] -> false
  | [ c1 ] -> false
  | c1 :: c2 :: t -> String.sub c1 0 3 = String.sub c2 0 3

let split_pair p =
  match p.hand with
  | [] -> failwith "Not a pair"
  | [ c1 ] -> failwith "Not a pair"
  | c1 :: c2 :: t ->
      {
        p with
        hand = [ c1 ];
        value = convert_cname_to_cval c1;
        snd_hand = ([ c2 ], convert_cname_to_cval c2);
      }

let switch_hands p =
  {
    p with
    hand = fst p.snd_hand;
    value = snd p.snd_hand;
    snd_hand = (p.hand, p.value);
  }

let has_snd_hand p = snd p.snd_hand <> 0

let has_double p =
  List.length p.hand = 2 && (p.value = 9 || p.value = 10 || p.value = 11)

let double_bet p =
  if has_double p && p.doubled = false then
    { p with bet = p.bet + p.bet; doubled = true }
  else p
