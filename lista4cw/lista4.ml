type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let tree = Node(1, Node(2, Node(4, Empty, Empty ), Empty ), Node(3, Node(5, Empty, Node(6, Empty, Empty ) ), Empty ) );; 

let bfs bt =
  let rec tree_help tree acc =
    if tree = [] then acc
    else match List.hd tree with
    | Empty -> tree_help (List.tl tree) acc
    | Node(v,l,r) -> tree_help(List.tl tree@[l;r]) (v::acc) in
    tree_help [bt] [];;
bfs tree;;

let breadthBT (tree: 'a bt ) =
  let rec search queue=
    match queue with
      [] -> []
    | h::t -> match h with
         | Node(v,l,r) -> v :: search (t @ [l;r])
         |Empty -> search (t)
                             
  in 
  search [tree];;
 breadthBT tree;;


let iternal bt = 
  let rec help depth tree = 
    match tree with 
    | Empty -> 0
    | Node(v, l, r) -> depth + help (depth+1) l + help (depth+1) r
  in help 0 bt;;


iternal tree;;

let externalDepth bt = 
  let rec help depth tree = 
    match tree with 
    | Empty -> depth
    | Node(v,l,r) -> help (depth+1) l + help (depth+1) r
  in help 0 bt;;

  externalDepth tree;;

type 'a graph = Graph of ('a -> 'a list);;
let g = Graph (function 
  0 -> [3]
  | 1 -> [0;2;4]
  | 2 -> [1]
  | 3 -> []
  | 4 -> [0;2]
  | n -> failwith("graf of given node does not exist")
);;


let depthSearch (Graph g) startNode =   
  let rec search visited graph = 
    match graph with 
    | [] -> []
    | h::t -> if List.mem h visited then search visited t
    else h :: search(h::visited)(g h@t)
  in search [] [startNode];;

  depthSearch g 4;;

  

   type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;; 
   let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;
    let rec ltake = function 
    | (0,_) -> []
    | (n, LCons(x, lazy xs)) -> x::ltake(n-1,xs);;

   let rec l_repeat num lis = 
    let rec helper reapt_num rest = 
      match (reapt_num, rest) with 
      | (_, LNil) -> LNil
      | (0, LCons(_, lazy tail)) -> helper num tail
      | (num, LCons(head, lazy tail)) -> LCons(head, lazy(helper (num-1) rest)) 
    in helper num lis;; 

    let res = l_repeat (4) (lfrom 10);;
    ltake (30,res);;


let lazy_fib =
    let rec fib_help a b =
        LCons(a, lazy(fib_help b (a + b)))
    in fib_help 1 1;; 
ltake (10,lazy_fib);;

type 'a ibt = LEmpty | LNode of 'a * (unit -> 'a ibt)* (unit -> 'a ibt);;
let rec lTree n =
	LNode(n, (function () -> lTree (2 * n)), function () -> lTree (2 * n + 1));; 


