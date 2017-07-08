package chapter3

import scala.annotation.tailrec

/**
  * Created by couto on 21/06/17.
  */

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  //Companion object
  object List {


    //apply method (create)
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def sum(ints: List[Int]): Int = ints match {
      case  Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ints: List[Int]) : Double = ints match {
      case Nil => 1.0
      case Cons(x, xs) => x * product(xs)
    }

    def foldRight[A,B](as: List[A], z: B) (f: (A, B) => B) : B = {
      as match {
        case Nil => z
        case Cons (x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    def sum2(ints: List[Int]): Int ={
      //foldRight(ints, 0)((x, y) => x + y)
      foldRight(ints, 0)(_ + _)

    }

    def product2(ints: List[Int]): Double ={
      //foldRight(ints, 1.0)((x, y) => x  * y)
      foldRight(ints, 1.0)(_ * _)

    }

    def append[A](a1: List[A], a2: List[A]): List[A] = {
      a1 match {
        case Nil => a2
        case Cons(h, t) => Cons (h, append(t, a2))
      }
    }

    //Exercise 3.1
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4,_))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      case Cons(h, t ) => h + sum(t)
      case _ => 101
    }

    //Exercise 3.2
    def tail[A](list: List[A]): List[A] = {
      list match {
        case Nil => Nil
        case Cons(x, t) => t
      }
    }

    //Exercise 3.3
    def setHead[A](a: A, list: List[A]): List[A] = {
      list match {
        case Nil => List(a)
        case Cons(x, t) => Cons(a,t)
      }
    }

    //Exercise 3.4
    def drop[A](list: List[A],n: Int): List[A] = {

      @tailrec
      def loop(rest: List[A], pending: Int): List[A] = {

        if (pending <=0 || rest == Nil) rest
        else rest match {
          case Nil => Nil
          case Cons(h, t) => loop(t, pending - 1 )
        }
      }
      loop (list, n)
    }


    //Exercise 3.5
    def dropWhileold[A](list: List[A], p: A => Boolean): List[A] = {
      @tailrec
      def loop(rest: List[A]): List[A] = {
        rest match {
          case Nil => Nil
          case Cons(h, t) => if (p(h)) loop(t)
                             else rest
        }
      }

      loop (list)
    }

    //Exercise 3.5 bis separate function to a second argument so it can infer the type by itself
    def dropWhile[A](list: List[A])(p: A => Boolean): List[A] = {
      @tailrec
      def loop(rest: List[A]): List[A] = {
        rest match {
          case Nil => Nil
          case Cons(h, t) => if (p(h)) loop(t)
          else rest
        }
      }

      loop (list)
    }

    //Exercise 3.6
    def init[A](l: List[A]): List[A] = {

      @tailrec
      def loop(acc: List[A], rest: List[A]) : List[A] = {

        rest match {
          case Nil => acc
          case Cons(h, t) => t match {
            case Nil => acc
            case _ => loop(append(acc, List(h)), t)
          }

        }
      }
      loop (List(), l)
    }

    //Exercise 3.7
    /*
    Arguments for f are evaluated before the call to the actual function. So we need to traverse the whole list.
     */

    //Exercise 3.8
    //see tests

    //Exercise 3.9
    def length[A](as: List[A]): Int = {
      foldRight(as, 0) ((x, y) => y + 1)
    }

    //Exercise 3.10
    @tailrec
    def foldLeft[A,B](as: List[A], z: B) (f: (B, A) => B) : B = {
//      @tailrec
//      def loop (rest: List[A], acc: B) : B = {
//        rest match {
//          case Nil => acc
//          case Cons (x, xs) => loop(xs, f(acc, x))
//        }
//      }
//      loop(as, z)
      as match  {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }

    //Exercise 3.11
    def sumLeft(ints: List[Int]): Int ={
      //foldLeft(ints, 0)((x, y) => x + y)
      foldLeft(ints, 0)(_ + _)

    }

    def productLeft(ints: List[Int]): Double ={
      //foldLeft(ints, 1.0)((x, y) => x  * y)
      foldLeft(ints, 1.0)(_ * _)
    }

    def lengthLeft[A](as: List[A]): Int = {
      foldLeft(as, 0) ((x, y) => x + 1)
    }

    //Exercise 3.12
    def reverse[A](as: List[A]): List [A] = {
//        as match {
//          case Nil => Nil
//          case Cons(h, t) => append(reverse(t), List(h))
//        }
//    foldRight(as, Nil: List[A]((a: A, b: List[A]) => append(b, List(a)))
//    foldRight(as, List[A]())((a: A, b: List[A]) => append(b, List(a)))
      foldRight(as, List[A]())((h, acc) => append(acc, List(h)))
//      foldLeft(as, List[A]())((acc,h) => Cons(h,acc))

    }

    //Exerise 3.13
    def foldLeftWithFoldRight[A, B](as: List[A], z: B) (f: (B, A) => B) : B = {
      foldRight(as, z)((a, b) => f(b, a))
    }

    def foldRightWithFoldLeft[A, B](as: List[A], z: B) (f: (A, B) => B) : B = {
      foldLeft(as, z)((b, a) => f(a, b))
    }


    //Exercise 3.14
    def appendfoldRight[A](a1: List[A], a2: List[A]): List[A] = {
      foldRight(a1, a2)(Cons(_,_))
    }

    def appendfoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
      foldRight(a1, a2)(Cons(_,_))
    }

    //Exercise 3.15
    def appendLists[A](as: List[List[A]]): List[A] = {
      foldRight(as, Nil: List[A])(append)
    }

    //Exercise 3.16
    def addOne(l: List[Int]): List[Int] = {
      foldRight(l, Nil: List[Int])((h,t) => Cons(h+1, t))
    }

    //Exercise 3.17
    def doubleToString(l: List[Double]) : List[String] = {
      foldRight(l, Nil: List[String])((h,t) => Cons(h.toString, t))
    }

    //Exercise 3.18
    def map[A,B](as: List[A])(f:A => B): List[B] = {
      foldRight(as, Nil:List[B])((h,t) => Cons(f(h), t))
    }

    //Exercise 3.19
    def filter[A](as:List[A])(f: A=> Boolean): List[A] = {
      foldRight(as, Nil:List[A])((h,t) => if (f(h)) Cons(h, t)
                                          else t )
    }

    //Exercise 3.20
    def flatMap[A,B](as:List[A])(f: A =>List[B]): List[B] = {
      appendLists(map(as)(f))
    }

    //Exercise 3.21
    def filterWithFlatMap[A](as:List[A])(f: A=> Boolean): List[A] = {
      flatMap(as)(a=> if (f(a)) List(a) else Nil)
    }

    def addLists(a1: List[Int], a2: List[Int]) : List[Int] = {
      (a1, a2) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addLists(t1,t2))
      }
    }


    def zipWith[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C) : List[C] = {
      (a1, a2) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
      }
    }


    def hasSubsequence[A] (sup: List[A], sub:List[A]) :Boolean = {

      def  loop(list: List[A], seq: List[A], success: Boolean) : Boolean = {

        (list, seq) match {
          case (_, Nil) => success
          case (Nil, _) => success
          case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) loop (t1, t2, true)
                                                else if (success) false
                                                          else  loop (t1, seq, false)
        }
      }

      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case _ => loop(sup, sub, false)

      }



    }

  }
