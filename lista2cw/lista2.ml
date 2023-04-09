let rec evenR n = if n=0 then true else oddR(n-1) and oddR n =if n=0 then false else evenR(n-1);;
 (* w ocmal gleboskosc stosue to 4, dlaczego *)
 (* evenR(3) -> oddR(2) -> evenR(1) -> oddR(0)(result true) *)

 let rec fib x = 
  if x = 0 then 0
  else if x = 1 then 1
  else fib(x-1) + fib(x-2);;

let rec fib2 n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib2(n-1) + fib2(n-2);;


let fibTail x =
  let rec fibTailHelper(x, preVal, acc) =
  if x = 0 then preVal
  else if x = 1 then acc
  else fibTailHelper(x-1, acc, acc+preVal)
in fibTailHelper(x,0,1);;

let fibTalMatch n =
  let rec helper(x, prev, acc) =
    match x with 
    0 -> 0
    | 1 -> 1
    | _ -> helper(x-1, acc, acc+prev)
  in helper(n, 0,1);;


  let root3 a =
    let rec root3Help(x) =
      if abs_float(x **3. -. a) <= 1e-15 *. abs_float(a) then x
      else root3Help(x +. (a /. x**2. -. x) /. 3.)
    in root3Help(if a <= 1. then a else a /. 3.);;

    (* let root3 a =
      let rec root3Helper x = 
        if abs_float(x ** 3. -. a) <= 1e-015 *. abs_float(a) then x
        else root3Helper(x +. (a /. x ** 2. -. x) /. 3.)
      in root3Helper(if a <= 1. then a else a /. 3.);; *)

  let root3 a = 
    let rec root3Help(x, a) = 
      if abs_float(x ** 3. -. a) <= 1e-015 *. abs_float(a) then x
      else root3Help(x +. (a /. x ** 2. -. x) /. 3., a)
    in if a > 1.  then root3Help(a /. 3., a) else  root3Help(a,a);;

    
    
    
  (* 4a *)
(* let l = [-2;-1;0;1;2] *)
(* val r = [_;_;0;_;_] *)
(* _::_::x::_ *)


(* let l2 = [(_,_); (x,_)]    *)
(* let [_; (x,_)] = [(1,2); (0,1);];; *)

let epsilon = 1.0e-15
let root3 x =
  let rec rootHelper (x, a) = 
  if abs_float(x *. x *. x -. a) <= epsilon *. abs_float(x) then x
  else rootHelper(x +. (a /. (x *. x) -. x) /. 3.0, a) 
in
  if x > 1.0 then rootHelper (x /. 3.0, x) else rootHelper (x, x);;
root3 27.0;;

(* // a' list -> a' list -> bool *)
let rec initSegment list seg = 
  match (list, seg) with
  | ([], []) -> true
  | (_, []) -> true
  | ([],_) -> false
  | (h1::t1, h2::t2) -> if h1=h2 then initSegment t1 t2 else false ;;

initSegment [1;2;3] [1;2];;

(* a'list * int * 'a-> a' list  *) 
let rec repalaceNth (xs, pos, el) =
  match (xs, pos) with 
  | ([], _) -> []
  | (h::t, 0) -> el::t
  | (h::t, pos) -> h::repalaceNth (t, pos-1, el);;

repalaceNth([1;2;3;4;], 1, 5);;

(* (a'*b'*c'->d')-> a' -> b' -> c' -> d' *)
let curry3 f x y z = f(x,y,z)

let uncyrry3 f(x,y,z) = f x y z;;


(* let rec sumProd xs = 
  match xs with 
  | [] -> (0,1)
  | h:: t -> let (s,p) = sumProd t in (h+s, h*p)

sumProd  *)

(* (0,1 ) - acc *)
let rec fold_left f acc lst = 
  match lst with 
  | [] -> acc
  | h::t -> fold_left f (f acc h) t

(* let sumProdc xs = List.fold_left (fun (s,p) h -> (s+h, p*h)) (0,1) xs;; *)

let sumProd list = List.fold_left (fun (s,i) h -> (s + h, i * h)) (0, 1) list;;

(* 
let  insertion_sort xs order = 
  let rec inser el lst = 
    match lst with 
    | [] -> el
    | h::t as lst -> if givenOrder h el then el :: lst else h :: inser el t
  in List.fold_left (fun acc newElement -> inser newElement acc) [] list;; *)


  
  (* let rec insertion_Sort l order =
    match l with
     | [] -> []
     | (h::t) -> insert h (insertion_Sort t)
  and insert el l =
    match l with
     | [] -> [el]
     | (h::tl) -> if order(el, h) then h::(insert el tl)
                          else el::h::tl ;; *)


let insertSort xs order = 
  let rec insert el lst = 
    match lst with 
    | [] -> el::[]
    | h::t -> if order el h then h :: insert el t else el :: lst
  in List.fold_left (fun acc newElement -> insert newElement acc) [] xs;;

insertSort [3;5;1;2;5;] givenOrder;;

let givenOrder a b = if a < b then true else false
let rec merge (l1 ,l2 ,order) =
  match (l1, l2) with
  | ([], _) -> l2
  | (_,[]) -> l1
  | (h1::t1, h2::t2) -> if order h1 h2 then h1::merge (t1 ,l2 ,order) else h2::merge(l1, t2, order);;

let rec split left right con = 
  if con = 0 then (List.rev left, right)
  else split(List.hd right :: left)(List.tl right) (con -1);;

let rec merge_sort lst order = 
  match lst with 
  | [] -> []
  | h::[] -> lst
  | _ -> let part = List.length lst / 2 in if part = 0 then lst else  let (left, right) = split [] lst part 
in merge(merge_sort left order, merge_sort right order, order);;

merge_sort [1;5;2;6;7;8;2;] givenOrder;;


