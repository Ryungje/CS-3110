(* list of hours worked by each person, such that the 1st, 2nd, and 3rd
   elements of each list respectively correspond to hours worked on MS1,
   MS2, and MS3*)
let phyllis = [ 7; 6; 5 ]
let james = [ 6; 5; 3 ]
let longyi = [ 4; 4; 2 ]
let lauren = [ 1; 5; 1 ]

(* get the sum of total hours worked*)
let hours_worked =
  List.map (List.fold_left ( + ) 0) [ phyllis; james; longyi; lauren ]
  |> List.fold_left ( + ) 0
