object Lista4 extends App {
  sealed trait bt[+A]

  case object Empty extends bt[Nothing]

  case class Node[+A](elem: A, left: bt[A], right: bt[A]) extends bt[A]

  val tt = Node(1,
    Node(2,
      Node(4,
        Empty,
        Empty
      ),
      Empty
    ),
    Node(3,
      Node(5,
        Empty,
        Node(6,
          Empty,
          Empty
        )
      ),
      Empty
    )
  )

  def breadthBT[A](tree: bt[A]): List[A] = {
    def search[A](visited: List[A], queue: List[bt[A]]): List[A] = {
      queue match
        case Nil => Nil
        case h :: t => h match {
          case Node(v, l, r) => v:: search(v :: visited, t ::: List(l, r))
          case _ => search(visited, t)
        }
    }

    search(List(), List(tree))
  }

  println(breadthBT(tt))

  // zadanie 4
  //a) ścieżka wewnętrzna
  def internal [A](tree: bt[A]):Int ={
    def helper[A](tree: bt[A],depth : Int):Int={
      tree match
        case Node(v,l,r)=> depth + helper(l,depth+1)+helper(r,depth+1)
        case Empty=> 0
    }
    helper (tree,0)
  }
  internal(tt)

  //b) ścieżka zewnętrzna
  def external [A](tree: bt[A]):Int ={
    def helper[A](tree: bt[A],depth : Int):Int={
      tree match
        case Node(v,l,r)=> helper(l,depth+1)+helper(r,depth+1)
        case Empty=> depth
    }
    helper (tree,0)
  }
  external(tt)

  sealed trait Graphs[A]
  case class Graph[A](succ: A=>List[A]) extends Graphs[A]

  val g = Graph((i: Int) => i match {
    case 0 => List(3)
    case 1 => List(0,2,4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0,2)
    case n => throw new Exception("Graph g: node „ + n" + " doesn't exist")
  })

  def depthSearch[A] (g: Graph[A]) (startNode: A): List[A] = {
    def search(visited: List[A])(toVisit: List[A]): List[A] = toVisit match {
      case Nil => Nil
      case h::t =>
        if (visited contains h) search(visited)(t)
        else h::search(h::visited)((g succ h)++  t )
    }
    search (Nil) (List(startNode))
  }

  println(depthSearch(g)(4))

}
