import javax.naming.spi.DirStateFactory.Result

object Lista2 extends App {
  def evenR(n:Int): Boolean =
    if ( n== 0) true else oddR(n-1)
  def oddR(n:Int):Boolean = if (n==0) false else evenR(n-1)

  println(evenR(3))
//  w scali glebosc stosue to 4
//  evenR(3) -> oddR(2) -> evenR(1) -> oddR(0)(result false)

  def fib(n:Int ): Int = {
    if (n == 0) return 0;
    if (n == 1) return 1;
    else fib(n-1) + fib(n-2)
  }
//  println(fib(42)) // duzo wolniejsze
  def fib2(n:Int ): Int = {
    n match {
      case 0 => 1
      case 1 => 1
      case _ => fib2(n-1) + fib2(n-2)
    }
  }

  def fibTail(n:Int):Int = fibTailHelper(n, 0,1)
  def fibTailHelper(i: Int, onePrev: Int, acc: Int):Int = {
    if(i == 0) onePrev
    if(i == 1) acc
    else fibTailHelper(i-1, acc, acc+onePrev)
  }

//  println(fibTail(42)) //szybsze

  val epsilon = 1.0e-15;
  def root3(x: Double): Double = root3Helper(if(x>1) x/3 else x, x);

  def root3Helper(x: Double, a: Double): Double = {
    if(math.abs(math.pow(x,3) - a) <= epsilon * math.abs(a)) x
    else root3Helper(x + (a/math.pow(x,2)-x)/3, a)
  }

  val root3Func: (Double) => Double = (x: Double) => root3FuncHelper(if(x>1) x/3 else x, x);
  val root3FuncHelper: (Double, Double) => Double = (x:Double, a: Double) => {
    if(math.abs(math.pow(x,3) - a) <= epsilon * math.abs(a)) x
    else root3FuncHelper(x + (a/math.pow(x,2)-x)/3, a)
  }
  println(root3Func(2))

  ////////////// zadanie 4  a
//   val l:List[Int] =  List(-2,-1,0,1,2)
//   List(_,_,0,_,_) = l

//  _::_::x::_
  //b) val l2 = [(1,2); (0,1)]
//  val bRes = List((_,_),(p,_))
//  _::List((0,_))

  def initSegment[A](list: List[A], seg: List[A]): Boolean = {
    (list, seg) match {
      case(Nil,Nil) => true
      case (Nil, _) => false
      case (_,Nil) => true
      case (h1::t1, h2::t2) => if(h1 == h2) initSegment(t1, t2) else false;
    }
  }
  println(initSegment(List(1,2,3), List(1,2,3)))

  def replaceNth[A](xs: List[A], n: Int, x: A): List[A] = {
    (xs, n) match {
      case (Nil, _) => Nil
      case (h::t, 0) => x :: t
      case (h::t, _) => h::replaceNth(t, n-1, x)
    }
  }

  println(replaceNth(List('a', 'b', 'c', 'd'), 1, 'f'))

  def sumProd(xs: List[Int]) = xs.foldLeft((0,1))((acc, h) => (acc._1 +h, acc._2 *h))
  println(sumProd(List(1,2,3,4)))

  def ord(a:Int, b:Int) = if (a< b) true else false
  def insertionSort(list: List[Int], order:(Int, Int) => Boolean):List[Int] = {
    def insert(element:Int, lst:List[Int]):List[Int] =
      lst match {
        case Nil => element::Nil
        case h :: t => if (order(h, element)) h :: insert(element, t)  else element :: lst
      }
    def sort(list: List[Int], acc: List[Int]): List[Int] =
      if (list == Nil) acc
      else sort(list.tail, insert(list.head, acc))

    sort(list, Nil)
  }
  println(insertionSort(List(3,4,1,2,5), ord))


  def merge(l1: List[Int], l2:List[Int], order: (Int, Int) => Boolean): List[Int] = (l1, l2) match {
    case (Nil,_) => l2
    case (_, Nil) => l1
    case (h1::t1, h2::t2) => if (order(h1,h2)) h1::merge(t1, l2, order)
    else h2::merge(l1, t2, order)
  }
  def mergeSort(lst: List[Int], order: (Int, Int) => Boolean): List[Int] = {
    lst match {
      case Nil => Nil
      case h::Nil => lst
      case _ =>
        val (l1, l2) = lst.splitAt(lst.length/2)
        merge(mergeSort(l1, order), mergeSort(l2, order), order)
    }
  }

  println(mergeSort(List(4,6,1,2,4,2), ord))

}
