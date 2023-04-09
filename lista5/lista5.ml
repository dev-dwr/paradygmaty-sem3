(* zadanie 1 *)
let point2D (x: float) (y: float) = (x, y);;
let point1 = point2D 0. 0.;;
let point2 = point2D 3. 4.;;
let distanceWithTuple a b = let (oSXa, osYa) = a and (osXb, osYb) = b 
in sqrt((osYb -. osYa) ** 2. +. (osXb -. oSXa) ** 2.);;
distanceWithTuple point1 point2;;

type point2DType = {x: float; y: float};;
let pointr1 = {x = 0.; y = 0.};;
let pointr2 = {x = 3.; y = 4.};;
let distanceWithType a b = sqrt((b.y -. a.y) ** 2. +. (b.x -. a.x) ** 2.);;
distanceWithType pointr1 pointr2;;


type pointGeneral = {arr: float list};;
let point31 = {arr = [0.; 0.; 0.]};;
let point32 = {arr = [1.; 1.; 1.]};;
let distanceGeneral a b = if  List.length a.arr != List.length b.arr then None else
  let rec distanceHelper arr1 arr2  sum =
    match (arr1, arr2) with
    | ([], []) -> sqrt(sum)
    | (h1::t1, h2::t2) -> distanceHelper t1 t2 (sum +. (h2 -. h1) ** 2.) in
    distanceHelper a.arr b.arr 0.;;

distanceGeneral point31 point32;;


(* zadanie2 types*)
(* gender true -> male else female *)

type personType = {
  firstname: string; 
  lastname: string; 
  age: int; 
  gender: bool; 
  shoe: int
};;

let personType1 = {
  firstname = "Jan";
  lastname= "Kowalski";
  age = 10;
  gender = true;
  shoe = 43;
};;

let personType2 = {
  firstname = "Piotr";
  lastname = "Nowak";
  age = 11;
  gender = true;
  shoe = 39;
};;
type partnership = {per1: personType; per2: personType};;
let partners = {per1 = personType1; per2 = personType2};;
let whoIsYounger persons = if persons.per1.age < persons.per2.age then persons.per1.firstname else persons.per2.firstname;;
whoIsYounger partners;;


(* zadanie2 tuple  *)
let person (firstname: string) (lastname: string) (age: int) (gender: bool) (shoeSize: int) 
= (firstname, lastname, age, gender, shoeSize);;

let person1 = person "Dawid" "Rymarczyk" 20 true 44;;
let person2 = person "Anja" "Kowalska" 22 false 38;;

let partnership per1 per2 = (per1, per2);;
let given_partners = partnership person1 person2;;

let whoIsYounger people = let ((firstname1,_,age1,_,_), (firstname2,_,age2,_,_)) = people in if age1 < age2 
  then firstname1 else firstname2;;
whoIsYounger given_partners;;


(* zadanie3 *)

type weekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday;;
let friday = Friday;;

let weekDToString day = 
  match day with
  | Monday -> "Poniedziałek"
  | Tuesday -> "Wtorek"
  | Wednesday -> "Środa"
  | Thursday -> "Czwartek"
  | Friday -> "Piątek"
  | Saturday -> "Sobota"
  | Sunday -> "Niedziela";;
  
weekDToString friday;;


let nextDay day = 
  match day with
  | Monday -> Tuesday
  | Tuesday -> Wednesday
  | Wednesday -> Thursday
  | Thursday -> Friday
  | Friday -> Saturday
  | Saturday -> Sunday
  | Sunday -> Monday;;
nextDay friday;;

(* zadanie4 *)

type 'a maybe = Just of 'a | Nothing;;
let safeHead arr =
  match arr with
  | [] -> Nothing
  | h::t -> Just(h);;

safeHead [];;
safeHead [5;4;3;2;1];;


(* zadanie5 *)
type cuboid = { x: float; y: float; z: float };;
type cone = { r: float; h: float };; 
type sphere = { r: float };;
type cylinder = { r: float; h: float };;
type 'a solidFigure = Cylinder of cylinder | Sphere of sphere | Cone of cone | Cuboid of cuboid;;

let volume figure =
  match figure with
  | Cuboid c -> c.x *. c.y *. c.z
  | Cone c -> (3.14 *. c.r ** 2. *. c.h) /. 3.
  | Sphere b -> (4. *. 3.14 *. b.r ** 2.) /. 3.
  | Cylinder c -> 3.14 *. c.r ** 2. *. c.h;;


let kula = Sphere({r = 1.});;
volume kula;;
let walec = Cylinder({r = 1.; h = 1.});;
volume walec;;
let prosto = Cuboid({x = 1.; y = 1.; z = 1.});;
volume prosto;;
let stozek = Cone({r = 1.; h = 1.});;
volume stozek;;