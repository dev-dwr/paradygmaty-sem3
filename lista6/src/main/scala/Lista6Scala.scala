import scala.collection.mutable

object Lista6Scala extends App {
  //zad1
  def stirling(n: Int, m: Int): Int =
    (n, m) match {
      case (_, 1) => 1
      case (n, m) => if (n == m) {
        1
      } else {
        stirling(n - 1, m - 1) + m * stirling(n - 1, m)
      }
    }

  println(stirling(7,4))
  val map = new mutable.HashMap[(Int, Int), Int]
  def memoized_stirling(n: Int, m: Int): Int = {
    (n, m) match {
      case (_, 1) => 1
      case (n, m) => if (n == m) 1 else {
        if (map.contains((n, m))) map((n, m))
        else {
          val res = stirling(n - 1, m - 1) + m * stirling(n - 1, m);
          map.addOne((n, m), res)
          res
        }
      }
    }
  }

  println(memoized_stirling(7,4))

  //zadanie 2
  def fibo(x: Int): Int =
    x match {
      case 0 => 0
      case 1 => 1
      case x => fibo(x - 1) + fibo(x - 2)
    }

  val cache = new mutable.HashMap[Int, Int]

  def make_memoize(fu: Int => Int): (Int => Int) = (x: Int) => {
    if (cache.contains(x)) {
      cache(x)
    } else {
      val res = fu(x)
      cache.addOne(x, res)
      res
    }
  }

  val memoized_fib = make_memoize(fibo)(10)
  println("fibo: " + memoized_fib)

  //zadanie 3
  lazy val lazy_s: Unit = println(stirling(5, 2))
  println("test")
  //  println(lazy_s)

  //zadanie 4
  trait Sequence[A]
  case class Cons[A](num: A, fu: () => Cons[A]) extends Sequence[A]

  def natural(n: Int): Cons[Int] = Cons(n, () => natural(n + 1))

  //sumowanie strilinga
  def bell(n: Int, k: Int): Cons[Int] = Cons(stirling(n, k), () => bell(n + 1, k))


  def streamHead[A](stream: Cons[A]): A =
    stream match {
      case Cons(h, t) => h
    }

  def streamTail[A](str: Cons[A]): (() => Cons[A]) =
    str match {
      case Cons(h, t) => t
    }

  def streamList[A] (s: Cons[A], n: Int): List[A] = {
    def listHelper[A] (s: Cons[A], n: Int, acc: List[A]): List[A] = {
      n match {
        case 0 => acc
        case n => listHelper (streamTail(s)(), n - 1, acc :+ streamHead(s))
      }
    }
    listHelper(s, n, List())
  }
  println("lists" + streamList(bell(7, 4), 2))

  def streamEvenNum[A] (str : Cons[A], num: Int): List[A] = {
    def help(s: Cons[A], n: Int, acc: List[A]): List[A] = {
      n match {
        case 0 => acc
        case el => if (el%2 ==0){
          val thunk = streamTail(s)
          help(thunk(), el-1, acc :+ streamHead(s))
        }
          else help(streamTail(s)(), el-1, acc)
      }
    }
    help(str, num*2, List()); //in order to have the same about as it was at the beginnig if i reject every second elemnt
  }
  println("even" + streamEvenNum(bell(7, 4), 3));


  def streamAband[A] (str: Cons[A], num: Int): Cons[A] =
    num match {
      case 0 => str
      case n => streamAband(streamTail(str)(), n - 1)
    }

  def mergeStream[A, B] (stream1: Cons[A], stream2: Cons[B], n: Int): List[(A, B)] = {
    def merge[A, B] (s1: Cons[A], s2: Cons[B], n: Int, acc: List[(A, B)]): List[(A, B)] =
      n match {
        case 0 => acc
        case n => merge(streamTail(s1)(), streamTail(s2)(), n - 1, acc :+ (streamHead(s1), streamHead(s2)))
      }
    merge(stream1, stream2, n, List())
  }

  println(mergeStream(natural(2), bell(7,4), 3))

  def double (x:Int) = x *2;
  def streamPattern[A](str: Cons[A], f:(x:A) => A): Cons[A] = {
      str match {
        case Cons(h, t) => Cons(f(h), () => streamPattern(t(), f))
      }
  }
  val res = streamPattern(bell(7,4), double)
  println(streamList(res,10))
}


