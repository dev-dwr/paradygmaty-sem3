(* zadanie1 *)
let stirling (n, m) =
  let rec strilingHelp (num1, num2) =  
  match (num1,num2) with 
  | (_, 1) -> 1
  | (x,y) -> if x=y then 1 else strilingHelp(x-1, y-1) + y*strilingHelp(x-1, y)
  in strilingHelp(n,m);;
stirling (5,2);;

let my_hash = Hashtbl.create 123456;;
let memoized_stirling (n, m) =
  let rec memoized_stirling_help (num1, num2) =  
  match (num1,num2) with 
  | (_, 1) -> 1
  | (x,y) -> if x=y then 1
  else if Hashtbl.mem my_hash (x,y) then 
    Hashtbl.find my_hash (x,y)  
  else 
    let res = memoized_stirling_help(x-1, y-1) + y*memoized_stirling_help(x-1, y) in 
    Hashtbl.add my_hash (n,m) res;
  res
in memoized_stirling_help(n,m);;

  memoized_stirling (5,2);;



  (* zadanie2 *)
  let rec fibo x =
    match x with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibo (x - 1) + fibo (x - 2);;

  let hash = Hashtbl.create 123456;;
  let make_memoize pureFunction = fun x -> if Hashtbl.mem hash x then Hashtbl.find hash x 
  else 
    let res = pureFunction x in 
    Hashtbl.add hash x res; 
    res;;
  
let memoized = make_memoize fibo 6;;

(* zadanie3  *)
let lazy_striling = lazy (stirling (7,4));;
print_string "test";;
print_int (Lazy.force lazy_striling);;


(* zadanie4  *)
type 'a sequence = Cons of 'a * (unit -> 'a sequence);;
let rec natural n = Cons (n, fun () -> natural(n+1));;
let rec bell n k = Cons(stirling (n,k), fun () -> bell (n+1) k );;

let stream_head (stream: ('a sequence)) = 
  match stream with 
  | Cons(h,t) -> h;;

  stream_head (bell 7 4);;

let stream_tail (s: ('a sequence)) =
  match s with
  | Cons (h, t) -> t;;
let thunk = stream_tail (bell 7 4);;
thunk ();;

let stream_list (stream: 'a sequence) number =
  let rec help s n acc =
    match n with
    | 0 -> acc
    | n -> let thunk = stream_tail s in help (thunk()) (n - 1) (acc@[stream_head s])
   in
    help stream number [];;
stream_list (bell 7 4) 5;;

let stream_second_el_list (stream: 'a sequence) number =
  let rec help s n acc =
    match n with
    | 0 -> acc
    | n -> let thunk = stream_tail s in 
    if n mod 2 = 0 then help (thunk ()) (n - 1) (acc@[stream_head s]) else 
      help (thunk ()) (n - 1) acc in
      help stream number [];;
stream_second_el_list (bell 7 4) 5;;

let rec stream_aband ((stream: 'a sequence), number) =
  match number with
  | 0 -> stream
  | n -> let thunk = stream_tail stream in stream_aband (thunk (), n - 1);;
stream_aband ((bell 7 4), 1);;

let stream_merge (s1: 'a sequence) (s2: 'a sequence) n =
  let rec merge (s1: 'a sequence) (s2: 'a sequence) n acc =
    match n with
    | 0 -> acc
    | n -> let thunk1 = stream_tail s1 and thunk2 = stream_tail s2 
  in 
    merge (thunk1 ()) (thunk2 ()) (n - 1) (acc@[(stream_head s1, stream_head s2)]) 
  in
  merge s1 s2 n [];;
stream_merge (natural 1) (bell 7 4) 3;;


let double x= x*2;;
let rec stream_pattern (stream: 'a sequence) u_function =
  match stream with 
  | Cons(h,t) -> Cons(u_function h, fun () -> stream_pattern (t()) u_function);;
let response = stream_pattern (bell 7 4) double;;
stream_list response 10;;