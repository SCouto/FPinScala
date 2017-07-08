package chapter2

import scala.annotation.tailrec

/**
  * Created by couto on 21/06/17.
  */
object Chapter2 {


  //Exercise 2.1
  def fib(n: Int): BigInt = {

    @tailrec
    def loop (acc1: BigInt, acc2: BigInt, x: Int) : BigInt = {

      if (x == n) acc1 + acc2
      else loop (acc2, acc1+ acc2, x+1)

    }

    if (n<2) n
    else loop (0, 1, 2)
  }

  //Exercise 2.2
  def intOrdering(a: Int, b: Int) : Boolean = a <= b
  def stringOrdering(a: String, b: String) : Boolean = a <= b
  def booleanOrdering(a: Boolean, b: Boolean) : Boolean = a <= b


  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop (rest: Array[A]): Boolean = {
      if (rest.isEmpty || rest.size == 1) true
      else if (!ordered(rest.head, rest.tail.head)) false
      else loop(rest.tail)
    }
    loop (as)
  }


  //Exercise 2.3
  def curry[A,B,C] (f: (A,B) => C) : A => (B => C) = a => b =>f(a,b)

  //Exercise 2.4
  def uncurry[A,B,C] (f: A => B => C) : (A,B) => C =  (a,b) => f(a)(b)

  //Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
    //g andThen f
    //f compose g

}
