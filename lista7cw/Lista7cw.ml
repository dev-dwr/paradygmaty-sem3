(* zadanie1 *)
(* a *)
module type QUEUE_FUN = sig   
  type 'a t   
  exception Empty of string   
  val empty: unit -> 'a t   
  val enqueue: 'a * 'a t -> 'a t   
  val dequeue: 'a t -> 'a t           
  val first: 'a t -> 'a   
  val isEmpty: 'a t -> bool
 end;;

 module ListQueue : QUEUE_FUN = 
 struct
   type 'a t = 'a list ;;
   exception Empty of string;;
   
   let isEmpty (q: 'a t) = if q = [] then true else false
   
   let first (q: 'a t) = 
    match q with 
    | [] -> raise (Empty "queue is empty")
    | head::tail -> head;;

    let dequeue (q: 'a t) = 
      match q with 
      | [] -> []
      | head::tail -> tail;;
    
    let empty () = [];;

    let enqueue (el , q) = el :: q;;
 end;;

 let q = ListQueue.enqueue (1, ListQueue.enqueue(2, ListQueue.empty()));;
 ListQueue.isEmpty q;;

 (* b *)
 module ListQueue2 : QUEUE_FUN = 
 struct
   type 'a t = ('a list * 'a list);;
   exception Empty of string;;
   
   let isEmpty (q: 'a t) = if fst q = [] then true else false
   
   let makeNormal (q, endOfQ) = match (q, endOfQ) with
   | ([], endQueue) -> (List.rev endQueue, [])
   | queue -> queue

   let first (q: 'a t) = 
    match fst q with 
    | [] -> raise (Empty "queue is empty")
    | head::tail -> head;;

    let dequeue (q: 'a t) = 
      match q with 
      | ([], _) -> ([],[])
      | (head::tail, tailQ) -> makeNormal(tail, tailQ);;
    
    let empty () = ([],[]);;

    let enqueue (el , q) = makeNormal(fst q, el :: snd q);
 end;;

 let q3 = ListQueue2.enqueue (1, (ListQueue2.empty()));;