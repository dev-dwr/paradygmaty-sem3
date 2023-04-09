import scala.annotation.tailrec

object Task6 extends App {
  println("Give me numbers in order to create list (write '-1' in order to leave)");
  var givenList: List[Int] = List()
  var continue = true
  while (continue){
    val number = scala.io.StdIn.readInt()
    if(number == -1){
      continue = false
    }else{
      givenList = givenList :+ number;
    }
  }

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
  val res = remDup(givenList);
  println("Result list elements without dupli: ")
  res.foreach(println)
}
