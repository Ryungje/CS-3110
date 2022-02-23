type command =
  | Hit
  | Stand
  | Play
  | Quit

exception Empty
exception Malformed

let parse_number i =
  let word_list =
    List.filter (fun x -> x <> "") (String.split_on_char ' ' i)
  in
  if word_list = [] then raise Empty
  else if List.length word_list > 1 then raise Malformed
  else
    match int_of_string (List.hd word_list) with
    | (n : int) when n >= 1 -> n
    | exception _ -> raise Malformed
    | _ -> raise Malformed

let parse_name n n_list =
  let word_list =
    List.filter (fun x -> x <> "") (String.split_on_char ' ' n)
  in
  if word_list = [] then raise Empty
  else
    let name = String.concat " " word_list in
    if List.exists (fun x -> x = name) n_list then raise Malformed
    else n_list @ [ name ]

let parse_command str =
  let word_list =
    List.filter (fun x -> x <> "") (String.split_on_char ' ' str)
  in
  if word_list = [] then raise Empty
  else if List.length word_list > 1 then raise Malformed
  else
    match List.hd word_list with
    | "hit" -> Hit
    | "stand" -> Stand
    | "play" -> Play
    | "quit" -> Quit
    | _ -> raise Malformed