import scala.annotation.tailrec

object Task4 extends App {
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

  def reverse[A](xs:List[A]): List[A] = reverseHelper(xs, List());
  @tailrec
  def reverseHelper[A](arr: List[A], result: List[A]): List[A] = {
    if (arr == List()) result
    else reverseHelper(arr.slice(0, arr.length-1), result.appended(arr.last))
  }
  val res = reverse(givenList);
  println("Given list is: ")
  res.foreach(print)
}
