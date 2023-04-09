import Lista5.WeekDay.{Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday}
//zadanie 1 tylko bazujac na krotkach, typ zminno przeciwnkowy pojedynczej precyzji
//zadanie 1 f odleglosci
//jak zmodyfikowac program aby bylo n-wymiarowa przesten

//zadanie 2 komponowanie struktru, korzystajac z rekordow lub krotek
// scala -> klase
//  typ iloczynowy Person osoba stanowi zlozenie
// koljeny typ to partnership kompozycja - zrobic 2 sposoby uzywac krotki 2 sposob klasy
// funkcja destrukturyzuje struktore i odwoluje sie do parametrow pol osoby

//zadanie 3
// typ sumaryczny reporezuntujacy dzien tyognodnia type dla ocaml
// enum dla scali
// dwie funcje dzialajace na tym typie
// 1 zwroci ciag znakow dla Monday zwrocimy string poniedzialek
// 2 funkcja przyjmuje dzien tygodnia i zwraca koljeny dzien tygodnia
// zadanie 4
// Typ Option wlasna opcja
//zadanie 5 skonstuj solidFigure type
object Lista5 extends App {
  //zadanie 1
  case class Point2D(x: Double, y: Double)

  val first = Point2D(0.0, 0.0)
  val second = Point2D(3.0, 4.0)

  def distance(point1: Point2D, point2: Point2D): Double = math.sqrt(math.pow((point1.x - point2.x), 2) + math.pow(point1.y - point2.y, 2))

  def distance2(point1: (Double, Double), point2: (Double, Double)): Double = math.sqrt(math.pow((point1._1 - point2._1), 2) + math.pow(point1._2 - point2._2, 2))

  case class PointGeneral(arr: List[Double]);
  val pointOne1 = PointGeneral(List(0.0, 0.0, 0.0));
  val pointOne2 = PointGeneral(List(1.0, 1.0, 1.0));

  def distanceGeneral(p1: PointGeneral, p2: PointGeneral): Double = help(p1.arr, p2.arr, 0.0)

  def help(p1: List[Double], p2: List[Double], sum: Double): Double = {
    if (p1.length != p2.length) -9999.0;
    else {
      (p1, p2) match {
        case (Nil, Nil) => math.sqrt(sum)
        case (h1 :: t1, h2 :: t2) => help(t1, t2, sum + math.pow((h2 - h1), 2))
      }
    }
  }

  println(distanceGeneral(pointOne1, pointOne2));
  println(distance2((0.0, 0.0), (3.0, 4.0)))
  println(distance(first, second))


  //zadanie 2
  case class Person(name: String, lastName: String, age: Int, gender: Boolean, shoeSize: Int);
  val per1 = Person("Dawid", "Rym", 20, true, 44);
  val per2 = Person("Ann", "Sym", 22, true, 38);

  case class Partnership(par1: Person, par2: Person);
  val partners = Partnership(per1, per2);

  def whoIsYounger(par: Partnership): String = if (par.par1.age > par.par2.age) par.par2.name else par.par1.name

  println("younger is: " + whoIsYounger(partners));

  val personFunc = (firstName: String) => (lastName: String) => (age: Int) => (gender: Boolean) => (shoeSize: Int) =>
    (firstName, lastName, age, gender, shoeSize);
  val person1 = personFunc("Dawid")("Rym")(20)(true)(44);
  val person2 = personFunc("Daniel")("Hasz")(19)(true)(40);

  val partnership2 = (person1, person2);

  def whoYoungerTuple(tup1: ((String, String, Int, Boolean, Int), (String, String, Int, Boolean, Int))) = {
    val ((firstname1, _, age1, _, _), (firstname2, _, age2, _, _)) = tup1
    if (age1 > age2) firstname2
    else firstname1
  }

  println(whoYoungerTuple(partnership2))


  //zadanie 3
  enum WeekDay {
    case Monday
    case Tuesday
    case Wednesday
    case Thursday
    case Friday
    case Saturday
    case Sunday
  }

  val friday = Friday;

  def weekDatToString(day: WeekDay) = {
    day match {
      case Monday => "Pon"
      case Tuesday => "Wtr"
      case Wednesday => "Srd"
      case Thursday => "Czw"
      case Friday => "Piat"
      case Saturday => "Sob"
      case Sunday => "Niedz"
    }
  }

  def nextDay(day: WeekDay) = {
    day match {
      case Monday => Tuesday
      case Tuesday => Wednesday
      case Wednesday => Thursday
      case Thursday => Friday
      case Friday => Saturday
      case Saturday => Sunday
      case Sunday => Monday
    }
  }

  println(weekDatToString(friday));
  println(nextDay(friday));

  //zadanie 4
    trait Maybe;
    case class Just[A](value: A) extends Maybe;
    case class Nothing() extends Maybe;
//  enum Maybe {
//    case Just[A](value: A)
//    case Nothing
//  }

  def safeHead[A](arr: List[A]): Maybe = {
    arr match {
      case h :: t => Just(h);
      case Nil => Nothing();
    }
  }

  println(safeHead(List(1, 2, 3, 4)))
  println(safeHead(List()))
  //zadanie 5
//  enum SolidFigure {
//    case Cube(x: Double, y: Double, z: Double)
//    case Cone(r: Double, h: Double)
//    case Sphere(r: Double)
//    case Cylinder(r: Double, h: Double)
//  }
  trait Figure;
  case class Cuboid(x: Double, y: Double, z: Double) extends Figure;
  case class Cone(r: Double, h: Double) extends Figure;
  case class Sphere(r: Double) extends Figure;
  case class Cylinder(r: Double, h: Double) extends Figure;
  def volume(figure: Figure) = {
    figure match {
      case Cuboid(x, y, z) => x * y * z;
      case Cylinder(r, h) => Math.PI * math.pow(r, 2) * h
      case Sphere(r) => (4 * Math.PI * math.pow(r, 2)) / 3
      case Cone(r, h) => (Math.PI * math.pow(r, 2) * h) / 3
    }
  }
  val kula = Sphere(1.0)
  println(volume(kula));
  val walec = Cylinder(1.0, 1.0);;
  println(volume(walec));
  val prosto = Cuboid(1.0, 1.0, 1.0);;
  println(volume(prosto));
  val stozek = Cone(1.0, 1.0);;
  println(volume(stozek));
}
