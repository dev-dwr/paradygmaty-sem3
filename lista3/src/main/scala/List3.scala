import List3.filterOwn


//curring technika
object List3 extends App {
  val list = List(1,2,3,4)
//  def sqrList(xs: List[Int]): List[Int] = sqrListHelper(xs, List(), xs);
//  def sqrListHelper(xs: List[Int], result: List[Int], const: List[Int]): List[Int]  = {
//    if (xs==Nil) result
//    else if(math.pow(xs.head,3) >= const.sum) sqrListHelper(xs.tail, result, const)
//    else sqrListHelper(xs.tail, result ::: List(xs.head*xs.head), const)
//  }
def sqrList(xs: List[Int]): List[Int] = xs.filter(x => math.pow(x,3) <= xs.sum).map(x=>x*x)
  println("Zadanie 6:" + sqrList(list))

  def abbreviationHelper(value: String): String =
    value match {
      case "" => ""
      case _ => value.split(" ").map(el => el.substring(0,1)).mkString
    }
  println("Zadanie 5:" + abbreviationHelper("Zakld Uslug Komunalnych"))

//  def avg(xs: List[Int]): Float = avgHelper(xs, 0, xs.length);
//  def avgHelper(value: List[Int], sum: Float, length: Float): Float = {
//    if (value == List()) (sum/length)
//    else avgHelper(value.tail,  sum+value.head , length);
//  }
  def avg(xs: List[Int]): Float = (xs.sum).toFloat / (xs.length).toFloat
  println("zadanie 4:" + avg(list))

  def ourFunc(a:Int): Int = a * a;
  def mapOwn[A, B](xs:List[A],f: (A => B)): List[B] = mapOwnHelper(xs, f, List())
  def mapOwnHelper[A, B](xs:List[A], f: (A => B), res: List[B]): List[B] = {
    if (xs == Nil) res
    else mapOwnHelper(xs.tail, f, res ::: List(f(xs.head)))
  }
  println("zadanie 1: " + mapOwn(list, ourFunc))

  def predicate(x: Int):Boolean = if(x % 2 == 0) true else false;
  def filterOwn[A](xs:List[A], pred: A => Boolean): List[A] = filterOwnHelper(xs, pred, List())
  def filterOwnHelper[A](xs:List[A], pred: A => Boolean, res: List[A]): List[A] = {
    if (xs == Nil) res
    else if (pred(xs.head)) filterOwnHelper(xs.tail, pred, res ::: List(xs.head))
    else filterOwnHelper(xs.tail, pred, res)
  }
 println("zadanie 2: " + filterOwn(list, predicate))

  def + (a:Int, b:Int): Int = a + b;
  def ownReduce[A, B](xs: List[A], f: (A, B) => B, acc:B): B = {
    if (xs == Nil) acc
    else ownReduce(xs.tail, f, f(xs.head, acc))
  }
  println("zadanie 3: " + ownReduce(list, + , 0))
}
