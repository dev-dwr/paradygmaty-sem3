import scala.annotation.tailrec

object Task8 extends App {
  println("Give me a number")
  val x = scala.io.StdIn.readInt()
  def isPrime(x:Int) = isPrimeHelper(x, 2)
  @tailrec
  def isPrimeHelper(i: Int, acc: Int): Boolean={
    if (i < 2) false
    else if (i == 2) true
    else if (i % acc == 0) false
    else if (acc*acc > i) true
    else isPrimeHelper(i, acc+1)
  }
  val res = isPrime(x);
  if (res){
    println(x + " is prime number")
  }else {
    println(x + " is not prime number")
  }

}
