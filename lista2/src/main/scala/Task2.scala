import scala.annotation.tailrec

object Task2 extends App {
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
  def twoLast[A](xs: List[A]): Option[(A,A)] = {
    xs match {
      case Nil => None
      case h :: Nil => None
      case h::t:: Nil => Some(h,t)
      case h::t => twoLast(t)
    }
  }
  println("Last two elements are: ")
  println(twoLast(givenList))
}
