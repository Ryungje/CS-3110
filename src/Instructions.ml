open Yojson.Basic.Util

type terminology = {
  name : string;
  explanation : string;
}

type descrip = {
  blackjack : string;
  terminologies : terminology list;
}

let term_of_json j =
  {
    name = j |> member "name" |> to_string;
    explanation = j |> member "description" |> to_string;
  }

let descrip_of_json j =
  {
    blackjack = j |> member "blackjack" |> to_string;
    terminologies =
      j |> member "terminology" |> to_list |> List.map term_of_json;
  }

let description =
  try
    descrip_of_json
      (Yojson.Basic.from_file "data/GameDescriptions.json")
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let blackjack_descrip _ =
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "Objective of the game: ";
  print_endline (description.blackjack ^ "\n")

let terminology_descrip _ =
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "Definitions of BlackJack terminologies: \n";
  let rec helper term_list =
    match term_list with
    | [] -> ()
    | h :: t ->
        let _ = print_endline ("- " ^ h.name ^ ": " ^ h.explanation) in
        helper t
  in
  helper description.terminologies