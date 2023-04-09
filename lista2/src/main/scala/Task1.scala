object Task1 extends App {
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

  def oneLast[A](xs: List[A]): Option[A] = {
    xs match {
      case Nil => None
      case h:: Nil => Some(h)
      case h::t => oneLast(t)
    }
  }
  println("Last element from the list is: ")
  println(oneLast(givenList))

}
