
let curry (x,y) = x+y;;
let un x y = x+y;;

let log prefix = function date -> function text -> 
   "[" ^ prefix ^ "] " ^ date ^ "\t" ^ text;;

let warn = log "Warn";;
let date = warn "2022-10-26 01:45";;
let hello = date "hello";;
let test = warn ("2022") ("hi");;

print_endline test;;



let sqrList xs = 
  let filtered = List.filter (fun x -> x*x*x < List.fold_left (+) 0 xs) xs in
  let sqr = List.map (fun x -> x * x) in 
  sqr filtered;;
sqrList([1;2;3;4]);;


let abb str =  
    match str with 
     | "" -> ""
     | string -> let strArr = String.split_on_char ' ' string in
     let rec strHelp(s, acc) = 
      match s with 
      | [] -> acc
      | h::t -> strHelp (t, acc ^ String.sub h 0 1)
     in strHelp(strArr, "");;

abb "Zaklad Uslug Komunalnych";;

let avg xs = float_of_int(List.fold_left (+) 0 xs) /. float_of_int(List.length xs);;
avg [1;2;3;4]


let lis = [1;2;3;4];;
let mapFunc x = x*x;;
let ownMap xs f =
  let rec ownMapHelper(arr, func, res) = 
    if arr = [] then res
    else ownMapHelper(List.tl arr, func, res @ [func(List.hd arr)])
  in ownMapHelper(xs, f, []);;

ownMap lis mapFunc;;

let predicate x = x < 4;;
let ownFilter xs pred =
  let rec ownFilterHelp(arr, f, result) = 
    if arr = [] then result
    else if f (List.hd arr) then ownFilterHelp(List.tl arr, f, result @ [List.hd arr])
    else ownFilterHelp(List.tl arr, f, result)
  in ownFilterHelp(xs, pred, []);;

ownFilter lis predicate;;


let ownReduce xs f acc = 
  let rec ownReduceHelp arr fu result = 
  match arr with 
  | [] -> result
  | h::t -> ownReduceHelp t fu (fu result h)
  in ownReduceHelp xs f acc;;

ownReduce [1;2;3;4] (+) 0;;