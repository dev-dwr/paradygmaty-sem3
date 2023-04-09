import scala.annotation.tailrec

object Lista2 extends App {

  println("lista2")
  ((x:Int) => x+x)(6) // f anonimowa
  val varFunc = ((x:Int) => x+x)(6) // f anonimowa
  val f = (y:Int) => (x:Int) => x*x+y;
  f(2)(5)

  def succTail(n: Int): Int = {
    def succIter(n:Int, acc: Int): Int = {
      if(n==0) acc
      else succIter(n-1, acc+1);
    }
    succIter(n,1)
  }

  val xs = List("a", "b", "c");
  val List(x1,x2,x3) = xs
  val h::t = xs;
  // h -> a
  // t -> ["b", "c"]

  val (z,_) = (false, 10); //wildcard

  val x = (("smith", 25), true);
  val ((a,b),c) = x

  //match pattern
  def imply1(pb:(Boolean, Boolean)): Boolean = {
    pb match {
      case (false, false) => true
      case (false, true) => true
      case (true, false) => false
      case (true, true) => true
    }

    imply1(1>2, true)
  }

  def zip[A,B](xs: List[A], ys: List[B]): List[(A,B)] =
    (xs, ys) match {
      case (h1::t1, h2::t2) => (h1, h2)::zip(t1,t2)
      case _ => Nil
    }
  zip(List(1,2,3), List('a', 'b', 'c')) // List((1,'a'), (2, 'b'), (3, 'c'))



  def unzip[A,B](ps: List[(A,B)]): (List[A], List[B]) =
    ps match {
      case Nil => (Nil, Nil)
      //h1 -> 1
      //h2 -> 2
      //t -> List((3,4), (5,6), (7,8))
      case (h1, h2)::t => {val(xs1, xs2) = unzip(t); (h1::xs1, h2::xs2)}
    }

  unzip(List((1,2), (3,4), (5,6), (7,8))) //(List(1,3,5,7), List(2,4,6,8))

  def avg (p:(Double, Double)) =
    p match {
      case (x,y) if x == y => x
      case (x, y) => (x+y) /2.0
    }


  def isPrime(x:Int) = isPrimeHelper(x, 2)
  @tailrec
  def isPrimeHelper(i: Int, acc: Int): Boolean={
    if (i < 2) false
    else if (i == 2) true
    else if (i % acc == 0) false
    else if (acc*acc > i) true
    else isPrimeHelper(i, acc+1)
  }
  println(isPrime(4))

  def evenIndexList[A](xs: List[A]) = evenHelper(xs, List(), 1);
  @tailrec
  def evenHelper[A](arr: List[A], result:List[A], pos: Int): List[A] = {
    if (arr == List()) result
    else if(pos % 2 == 0) evenHelper(arr.tail, result.appended(arr.head), pos+1)
    else evenHelper(arr.tail, result, pos+1);
  }
  print(evenIndexList(List(1,2,3,4,5,6)))


  @tailrec
  def exists[A](num: A, arr: List[A]):Boolean= {
    arr match {
      case Nil => false
      case (h::t) => if (h==num) true else exists(num, t)
    }
  }
  def remDup[A](xs: List[A]):List[A] = remDupHelperList(xs, List())
  @tailrec
  def remDupHelperList[A](arr: List[A], res: List[A]): List[A] = {
    if (arr == List()) res
    else if(exists(arr.head, arr.tail)) remDupHelperList(arr.tail, res)
    else remDupHelperList(arr.tail, res ::: List(arr.head))
  }
  println(remDup(List(1,2,2,3,4,5,5)))

  def pali[A](xs: List[A]):Boolean = if(reverseHelper(xs, List()) == xs) true else false
  def reverse[A](xs:List[A]): List[A] = reverseHelper(xs, List());
  @tailrec
  def reverseHelper[A](arr: List[A], result: List[A]): List[A] = {
    if (arr == List()) result
    else reverseHelper(arr.slice(0, arr.length-1), result.appended(arr.last))
  }
  println(pali(List(1,2,1)))
  println(reverse(List(1,2,2)))

  def length[A](xs:List[A]): Int = lengthHelper(xs, 0)
  @tailrec
  def lengthHelper[A](arr: List[A], count: Int): Int = {
    if (arr == List()) count
    else lengthHelper(arr.tail, count+1)
  }

  println(length(List(1,2,3,4,5)))

  @tailrec
  def twoLast[A](xs: List[A]): Option[(A,A)] = {
    xs match {
      case Nil => None
      case h :: Nil => None
      case h::t:: Nil => Some(h,t)
      case h::t => twoLast(t)
    }
  }

  // stworzyc kazda funckja jako osoby program i obsluzyc wejscie i wyjscie stanradowe wprowadz input
  println(twoLast(List(1,2,4,5,4,5)))

  def oneLast[A](xs: List[A]): Option[A] = {
    xs match {
      case Nil => None
      case h:: Nil => Some(h)
      case h::t => oneLast(t)
    }
  }
  println(oneLast(List(1,2,3,4,5,6)))

}

