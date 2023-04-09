(* (function x -> x+x)6;;
let double = function x -> x+x;;

let f = function y -> function x -> x*x +y;;

let plus (x,y) = x+y;;

let rec succ_tail n = 
  let rec succ_iter(n, acc) = 
    if n = 0 then acc
    else succ_iter(n-1, acc+1)
  in succ_iter(n,1);;

let xs = ["a";"b";"c"];;
(* h -> "a"
t -> ["b";"c"] *)

let (z, _) = (false, 10);;

let x = (("SMith", 25), true);;
let ((n,w),b) = x;;
(* n smith, w 25, b bool *)

let imply1 pb = 
  match pb with
    (false, false) -> true
  | (false, true) -> true
  | (true, false) -> false
  | (true, true) -> true
;;


let rec zip(xs, ys) =
  match (xs, ys) with
  (h1::t1, h2::t2) -> (h1, h2)::zip(t1,t2)
  | _ -> [];;
 
  (* [(1,'a');(2,'b');(3,'c')] *)
  (* 1::[2;3];; -> [1;2;3];; *)
zip([1;2;3], ['a';'b';'c']);; 

let rec unzip ps =
  match ps with 
  [] -> ([], [])
  | (h1,h2)::t -> let (l1, l2) = unzip t in (h1::l1, h2::l2);;

let isLatin v = 
  match v with 
  'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false;;


let avg p =
  match p with
  (x,y) when x=y -> y
  | (x,y) -> (x +. y) /. 2.0;;

 avg(5., 5.);; *)

(* 
 let rec sqr_list li = 
  match li with
  [] -> []
  | h:: tail -> h*h :: sqr_list t;; *)


(* let isPrime x =
  match x with
  0 -> false
  | 1 -> false
  | _ -> let rec primerHelper(n,i) =
    match i with
    1 -> true
    | _ -> if n mod i = 0 then false else primerHelper(n,i-1) in
    primerHelper(x, x-1);;

let res = isPrime 4;;
Printf.printf "%B" res;; *)

let isPrime x = 
  let rec primeHelper(x,acc) =
    if x < 2 then false
    else if x = 2 then true
    else if x mod acc = 0 then false
    else if acc*acc > x then true
    else primeHelper(x, acc+1)
  in primeHelper(x, 2);;

let res = isPrime 8;;
Printf.printf "task 8 result: %B" res;;

let evenIndexList lis = 
  let rec evenIndexListHelper(arr, resultList, position) =
    if arr = [] then resultList
    else if position mod 2 = 0 then evenIndexListHelper(List.tl arr, resultList@[List.hd arr], position +1)
    else evenIndexListHelper(List.tl arr, resultList, position +1)
  in evenIndexListHelper(lis, [], 1);;
evenIndexList([1;2;3;4;5;6;7]);;

let rec existInArr (x, arr) =
  match arr with 
    [] -> false
    | h::t -> if h = x then true else existInArr(x, t);;

let removeDuplicates lis =
  let rec removeDuplicates(arr, resultList) = 
    if arr = [] then resultList
    else if existInArr(List.hd arr, List.tl arr) then removeDuplicates(List.tl arr, resultList)
    else removeDuplicates(List.tl arr, resultList@[List.hd arr])
  in removeDuplicates(lis, []);;

removeDuplicates([1;2;3;1;4;6;3]);;

let palindrome xs =
  let rec palindrome_helper(arr, acc)=
  if arr = [] then acc = xs
  else palindrome_helper(List.tl arr, List.hd arr::acc)
in palindrome_helper(xs, []);;

palindrome([1;2;1;]);;

let reverse xs =
  let rec reverse_helper(arr, acc) = 
    if arr = [] then acc
    else reverse_helper(List.tl arr, List.hd arr::acc)
  in reverse_helper(xs, []);;

reverse([1;2;3;4]);;


let length xs =
  let rec length_helper(arr, acc) = 
    if arr = [] then acc
    else length_helper(List.tl arr, acc+1)
  in length_helper(xs,0);;

length([1;2;3;4;5]);;

let rec two_el xs =
  match xs with
  | [] -> None
  | [x] -> None
  | [x;y] -> Some(x,y)
  | h::t -> two_el t;;
two_el([1;2;3;4;5]);;

let rec one_el xs = 
  match xs with
  | []  -> None
  | [x] -> Some(x)
  | h::t -> one_el t;;
one_el([1;2;3;4;5]);;
