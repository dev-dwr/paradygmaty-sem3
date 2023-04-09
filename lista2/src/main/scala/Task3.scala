import scala.annotation.tailrec

object Task3 extends App {
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

  def length[A](xs:List[A]): Int = lengthHelper(xs, 0)
  @tailrec
  def lengthHelper[A](arr: List[A], count: Int): Int = {
    if (arr == List()) count
    else lengthHelper(arr.tail, count+1)
  }
  println("Length of the list is equal to: ")
  println(length(givenList))
}
