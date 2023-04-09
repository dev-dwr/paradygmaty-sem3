(* zadanie1 *)
module type Point_Type = sig
  type 'a point = {x:'a; y:'a; z:'a};;
  type pointXYZ = FloatPoint of float point | IntegerPoint of int point;;
  val distance: pointXYZ -> pointXYZ -> float;;
end;;

module Point : sig
  type 'a point = {x:'a; y:'a; z:'a};;
  type pointXYZ = FloatPoint of float point | IntegerPoint of int point;;
  val distance: pointXYZ -> pointXYZ -> float;;
end 
= 
struct
type 'a point = {x:'a; y:'a; z:'a};;
type pointXYZ = FloatPoint of float point | IntegerPoint of int point 
  let distance (x: pointXYZ)(y: pointXYZ) = 
    match (x,y) with 
    | (FloatPoint x, FloatPoint y) -> sqrt((x.x-.y.x)**2.+.(x.y-.y.y)**2.+.(x.z-.y.z)**2.)
    | (IntegerPoint x, IntegerPoint y) -> sqrt ((float_of_int (x.x-y.x))**2.+.(float_of_int (x.y-y.y))**2.+.(float_of_int (x.z-y.z))**2.)
    | (FloatPoint x, IntegerPoint y) -> failwith "invalid"
    | (IntegerPoint x, FloatPoint y) -> failwith "invalid"
end;;
  

let p1 = Point.FloatPoint{x = 5.; y = 4.; z = 3.};;
let p2 = Point.FloatPoint{x = 0.; y = 0.; z = 0.};;
Point.distance p1 p2;;


(* zadanie2 *)
module type Segment_Type = sig
  open Point;;
  type segment = {x: pointXYZ; y: pointXYZ};;
  val length: segment -> float;;
end;;


module Segment : sig
  open Point;;
  type segment = {x: pointXYZ; y: pointXYZ};;
  val length: segment -> float;;
end
=
struct
open Point;;
type segment = {x: pointXYZ; y: pointXYZ};;
let length (segmentParam: segment) = distance segmentParam.x segmentParam.y;;
end;;

let param = Segment.{x = p1; y = p2};;
Segment.length param;; 

(* zadanie3 *)
module BT : sig 
  type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;
  val listTree: 'a tree -> 'a list;; 
  val add : 'a tree -> 'a -> 'a tree;;
  val postorder : 'a tree -> 'a list;;

  val deleteNode: int tree -> int -> int tree;;
  val minValue: int tree -> int;;
end
= 
struct
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;; 
let rec add (tt: 'a tree) num =
  match tt with
  | Leaf -> Node(num, Leaf, Leaf)
  | Node (value, l, r) -> if num > value then Node (value, l, add r num) else Node (value, add l num, r);;

let rec listTree (tt: 'a tree) = match tt with 
| Leaf -> []
| Node(v, l, r) -> 
  match (l,r) with 
  | (Leaf, Leaf) -> [v]
  | (Leaf, r) -> listTree r
  | (l, Leaf) -> listTree l
  | (l, r) -> listTree l @ listTree r;; 

let rec postorder (tt: 'a tree) = 
  match tt with
  | Leaf -> []
  | Node (v, l, r) -> postorder l @ postorder r @ [v];;

  let rec minValue (tt: 'a tree) = 
    match tt with 
    | Leaf -> 0
    | Node (v, l, r) -> if l = Leaf then v else minValue l;;
  
let rec deleteNode (tree:'a tree) (value: 'a) = 
    match tree with 
    | Leaf -> Leaf
    | Node (nodeValue, left, right) when value < nodeValue ->
          Node(nodeValue, (deleteNode left value), right)
    | Node (nodeValue, left, right) when value > nodeValue ->
          Node(nodeValue, left, (deleteNode right value))
    | Node (nodeValue, left, right) -> 
        if left = Leaf && right = Leaf then 
          Leaf
        else if left = Leaf then 
          right
        else if right = Leaf then
          left
        else 
          let newValue = minValue right in
          Node(newValue, left, (deleteNode right newValue))

end;;

(* let my_tree = BT.add (BT.add (BT.add (BT.add (BT.Node(1, Leaf, Leaf)) 2) 3) 4) 5;; *)
let my_tree = BT.add (BT.add (BT.Node(5,Leaf, Leaf)) 6) 3;;
BT.listTree my_tree;;
BT.postorder my_tree;;
let newtree = BT.deleteNode my_tree 3;;
BT.postorder newtree;; 

(* zadanie4 *)
(* a *)
module type Point_Type = sig
  type t
end;;

module Make_Point (Type_P : Point_Type) = 
struct
    type t = Type_P.t;;
    let make (x, y, z) : t Point.point = Point.{x;y;z};;
end;;

module Int_Point = Make_Point(
  struct
  type t = int;;
end);;

module Float_Point = Make_Point(
  struct
  type t = float;;
end);;
let po1 = Float_Point.make (3.0, 2.0, 1.0);;


(* b *)
module type Segment_Type = sig
  type segment = SegmentT of Point.pointXYZ * Point.pointXYZ
  val length: segment -> float
end;;

module Segment : Segment_Type = 
struct
  type segment = SegmentT of Point.pointXYZ * Point.pointXYZ;;
  let length seg = let SegmentT(p1, p2) = seg in Point.distance(p1) (p2);;
end;;

module Make_Segment(Segme: Segment_Type) = struct
  let make (p1,p2) = Segme.SegmentT(p1, p2);;
end;;

module Segme = Make_Segment(Segment);;

let p3 = Point.FloatPoint{x = 5.; y = 4.; z = 3.};;
let p4 = Point.FloatPoint{x = 0.; y = 0.; z = 0.};;
let our_segment = Segme.make (p3,p4);;

(* c *)
module type Translation_Type = 
sig
  type unitType = Segme of Segment.segment | Point of Point.pointXYZ
  type 'a trans = { x: 'a; y: 'a; z: 'a }
  val translate: unitType -> int trans -> unitType
end;;


module Translation: Translation_Type = 
struct
  type unitType = Segme of Segment.segment | Point of Point.pointXYZ
  type 'a trans = { x: 'a; y: 'a; z: 'a }

  let translate (unitType: unitType) (transaltion) = 
    match unitType with
    | Segme seg -> (let Segment.SegmentT(p1, p2) = seg in 
        match (p1, p2) with 
        (IntegerPoint x, IntegerPoint y) ->
          Segme(Segment.SegmentT(IntegerPoint{x= x.x+ transaltion.x; y = x.y+ transaltion.y; z = x.z + transaltion.z}, IntegerPoint{x =y.x+ transaltion.x; y = y.y+transaltion.y; z = y.z+ transaltion.z})))
    | Point poi ->  match poi with 
          | Point.IntegerPoint x -> Point(Point.IntegerPoint{ x = x.x+ transaltion.x; y = x.y+ transaltion.y; z = x.z + transaltion.z});;      
end;;

module Translate_Point(T: Translation_Type) = 
struct
  let pointInt a b c = T.Point(IntegerPoint{x = a; y = b; z = c})
  let translation a b c = T.{x=a; y=b; z=c}
  let translate p transaltion = T.translate p transaltion
end;;

module TranslatePoint = Translate_Point(Translation);;
let tPoint = TranslatePoint.translate (TranslatePoint.pointInt 0 0 0) (TranslatePoint.translation 1 1 1);;

module Translate_Segment(Type: Translation_Type) = 
struct
  let segment a b = Type.Segme(Segment.SegmentT(a,b))
  let translation a b c = Type.{x=a;y=b;z=c}
  let translate seg translation = Type.translate seg translation
end;;

module TranslateSegment = Translate_Segment(Translation);;
let translatedSegment = TranslateSegment.translate (TranslateSegment.segment (Point.IntegerPoint{x = 0; y = 0; z = 0}) 
(Point.IntegerPoint{x = 1; y = 1; z = 1})) (TranslateSegment.translation 2 2 2);;

let y = 5;;
let f x = x+y;;
let g z = f (f z);;
let y = 2;;
let f x = x*5;;

let rec fold_right f l accu =
  match l with
   [] -> accu
   | a::l -> f a (fold_right f l accu)

f( f 1);; 

let rec tail_fact n acc =
  if n=0 then acc
  else tail_fact (n-1)(n*acc);;

let y = 4;;
let f x = x -y;;
let g z = f (f z);;
let y = 2;;
let f x = x*4;;
 g y;;