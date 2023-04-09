package org.pwr

import scala.math.pow

object List1 extends App {
  val x = 3+2;
  print(x);
  val str = "ala" + " ma";
  print(str);
  val result = {
    val y = x + x;
    val z = 2;
    {
      val x = 10.00
      x+y
    }
  } + 1;
  print(result);

  val tuple = (3+4, 3.0, 2<4)
  println("\n" + tuple._1)

  val xs = 1.0 :: x :: 2.5 :: Nil
  println(xs.head)
  println(xs.tail)

  val double = (x:Int) => 2*x;
  println(double(6))

  def twice(x:Int): Int = {
    return 2*x;
  }
  def test(x:Int):Boolean = if (x >4) true else false
  def fun(test: Int => Boolean, x: Int, y: Int, z :Int) = List(x,y,z).filter(test).sum

  println("fun" + fun(test, 5,6,7).toString)
//  def flatten1[A] (xss: List[List[A]]): List[A] = xss.flatten
  def flatten1[A] (xss: List[List[A]]): List[A] = flattenHelper(xss, List())
  def flattenHelper[A](xss: List[List[A]], result: List[A]): List[A] = {
    if(xss == List()) result
    else {
      flattenHelper(xss.tail, result:::xss.head)
    }
  }
  println(flatten1(List(List(5,6), List(1,2,3)))) //on

//  def count[A] (x: A, xs: List[A]): Int = xs.count(_ == x) //on
  def count[A] (x: A, xs: List[A]): Int = countHelper(x, xs, 0)
  def countHelper[A](x: A, xs: List[A], result: Int): Int= {
    if(xs == List()) return result
    if(xs.head == x) countHelper(x, xs.tail, result+1)
    else countHelper(x, xs.tail, result)
  }
  println(count('a', List('a', 'l', 'a', 'b', 'c')));

  println(replicate("la", 10))
//  def replicate[A](x:A, n:Int): List[A] = List.fill(n)(x)
  def replicate[A](x:A, n:Int): List[A] = helper(x, n, List[A]());

  def helper[A](a: A, i: Int, arr: List[A]): List[A] = {
    if(i == 0){
      arr
    }else{
      helper(a, i-1, arr.appended(a))
    }
  }


  def sqrList(xs:List[Int]): List[Int] = xs.map(x => x*x)
  val sqrList2 = (xs:List[Int]) => sqrHelper(xs, List())
  println(sqrList2(List(1,2,3,-4)))
  def sqrList3(xs:List[Int]): List[Int] = sqrHelper(xs, List[Int]())

  def sqrHelper(xs: List[Int], num: List[Int]): List[Int] = {
      if (xs == List()) {
        num
      } else {
        val sqrNum = xs.head*xs.head;
        sqrHelper(xs.tail, num.appended(sqrNum))
      }
  }

  println(palin(List('a','l', 'a')))
//  def palin[A](xs: List[A]): Boolean = xs == xs.reverse;
  def palin[A](xs: List[A]): Boolean = {
    val reversedList = palinHelper(xs, List());
    if(reversedList == xs) true
    else false
  };
  def palinHelper[A](list: List[A], reversedList: List[A]): List[A] = {
    if(list == List()) reversedList;
    else palinHelper(list.slice(0, list.length-1), reversedList.appended(list.last))
  }
  println(listLength(List(1,2,3,4,5)))
  def listLength[A](xs: List[A]) : Int = length(xs, 0)
  def length[A](xs: List[A], num :Int) : Int = {
    if(xs == List()) num
    else length(xs.tail, num+1)
  }
}

