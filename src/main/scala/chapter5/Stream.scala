package chapter5

import Stream._
import scala.annotation.tailrec

/**
  * Created by couto on 3/07/17.
  */
sealed trait Stream[+A] {
  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), Empty)
      case _ => Empty
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists0(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) || t().exists0(p)
      case _ => false
    }
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b)
    else Empty)
  }

  def headOption0: Option[A] = {
    this match {
      case Cons(h, _) => Some(h())
      case _ => None
    }
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t)
  }

  def append[B >: A](other: => Stream[B]): Stream[B] = {
    foldRight(other)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }

  def map[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), x) if x > 1 => Some(h(), (t(), x - 1))
      case _ => None
    }
  }


  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C) : Stream[C] = {
    unfold((this, other))  {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipWithAll[B, C](other: Stream[B])(f: (Option[A], Option[B]) => C) : Stream[C] = {
    unfold((this, other))  {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1,t1), Empty) => Some(f(Some(h1()), None), (t1(), empty[B]))
      case (Empty, Cons(h2,t2)) => Some(f(None, Some(h2())), (empty[A], t2()))
      case _ => None

    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    zipWithAll(s2)((_, _))
//    unfold((this, s2))  {
//      case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
//      case (Cons(h1,t1), Empty) => Some((Some(h1()), None), (t1(), empty[B]))
//      case (Empty, Cons(h2,t2)) => Some((None, Some(h2())), (empty[A], t2()))
//      case _ => None
//
//    }

  }


  def startsWith[A](s: Stream[A]): Boolean = {
    this.zipAll(s).takeWhile(a => a._2.isDefined && a._1.isDefined).forAll{case (l, r) => l.get == r.get}
  }

  def tails: Stream[Stream[A]] = {
    unfold(this)  {
      case Empty => None
      case str => Some((str, str drop 1))
    } append(Empty)
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    this.tails.exists(_.startsWith(s))
  }


    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
      foldRight((z, Stream(z))) ((a, b0) => {
        lazy val b = b0
        val b2 = f(a, b._1)
        (b2, cons(b2, b._2))
      })._2
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] ={
    cons(n, from(n+1))
  }

  def fibs: Stream[Int] ={
    def loop(acc1: Int, acc2: Int) : Stream[Int] = {
      cons(acc1, loop(acc2, acc1+acc2))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((value, state)) => cons(value, unfold(state)(f))
    }
  }

  def constantUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a, a))
  }

  def fromUnfold(n: Int): Stream[Int] ={
    unfold(n)(n => Some(n, n+1))
  }

  def fibsUnfold: Stream[Int] = {
    unfold((0, 1)){case (acc1, acc2) => Some(acc1, (acc2, acc1+acc2))}
  }


  def onesUnfold: Stream[Int] = {
    unfold(1)(_ => Some(1, 1))
  }


  }


