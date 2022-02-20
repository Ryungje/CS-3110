type card = {
  name : string;
  value : int;
}

type deck = card list

let reset n = raise (Failure "Not Implemented")