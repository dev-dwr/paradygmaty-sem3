open Format
open List
(*  *)

(* let result2 = count(arg1, arg2);;
(* let () = List.iter (printf "%c") result2;;  *)
let length =  List.length result2;;
printf "%d" length;; *)

(* let x = [3;4;5;7;8;9;0];;
print_int(List.length x) *)

(* 
let task4 ((fn: int -> bool), (x:int), (y:int), (z:int)) =
  (* let rec sumNumber list  =  
    match list with
    [] -> 0
    | x :: xs -> (x + sumNumber xs)
  in *)
  let sum = [0;] in
  if fn x then sum @ [x]
  if fn y then sum @ [y]
  if fn z then sum @ [z]
  else sum;; *)

let taskSum4 ((fn: int -> bool), (x:int), (y:int), (z:int)) : int=
  let list = [x; y; z] in 
  let filteredList = List.filter fn list in
  let sum = List.fold_left ( + ) 0 in
  sum filteredList;