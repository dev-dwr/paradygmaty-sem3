import scala.annotation.tailrec

object Task7 extends App {
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

  def evenIndexList[A](xs: List[A]) = evenHelper(xs, List(), 1);
  @tailrec
  def evenHelper[A](arr: List[A], result:List[A], pos: Int): List[A] = {
    if (arr == List()) result
    else if(pos % 2 == 0) evenHelper(arr.tail, result.appended(arr.head), pos+1)
    else evenHelper(arr.tail, result, pos+1);
  }

  val res = evenIndexList(givenList);
  println("Result list elements: ")
  res.foreach(println)
//  print(evenIndexList(List(1,2,3,4,5,6)))
}
