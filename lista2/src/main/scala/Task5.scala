import scala.annotation.tailrec

object Task5 extends App {
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

  def pali[A](xs: List[A]):Boolean = if(reverseHelper(xs, List()) == xs) true else false
  @tailrec
  def reverseHelper[A](arr: List[A], result: List[A]): List[A] = {
    if (arr == List()) result
    else reverseHelper(arr.slice(0, arr.length-1), result.appended(arr.last))
  }
  val res = pali(givenList);
  println("Given list is: ")
  println(res)
}
