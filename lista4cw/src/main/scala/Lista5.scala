object Lista5 extends App {

  def l_repeat[A](k:Int)(stream:LazyList[A]):LazyList[A] = {
    def helper(reap_num:Int, rest:LazyList[A]):LazyList[A] =
      (reap_num, rest) match {
      case (_, l) if l.isEmpty => LazyList.empty
      case (0, h #:: tail) => helper(k, tail)  //#:: for streams is the same for :: for list
      case (n, head #:: tail) => head #:: helper(n - 1, rest)
    }
    helper(k, stream)
  }
  val r = l_repeat(3)(LazyList('a', 'b', 'c', 'd'))
  println(r.toList)

  def fib (x:Int, y:Int): LazyList[Int] = x #:: fib(y, x+y)
  val lfib = fib(1,1);
}
