(* list of hours worked by each person, such that the 1st, 2nd, and 3rd
   elements of each list respectively correspond to hours worked on MS1,
   MS2, and MS3*)
let phyllis = [ 7; 2; 0 ]
let james = [ 6; 0; 0 ]
let longyi = [ 4; 0; 0 ]
let lauren = [ 1; 0; 0 ]

(* get the sum of total hours worked*)
let hours_worked =
  List.map (List.fold_left ( + ) 0) [ phyllis; james; longyi; lauren ]
  |> List.fold_left ( + ) 0
