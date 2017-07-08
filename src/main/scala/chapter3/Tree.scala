package chapter3

/**
  * Created by couto on 27/06/17.
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  //Exercise 3.25
  def size[A] (t: Tree[A]): Int = {

    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1+ size(l) + size(r)
    }

  }

  //Exercise 3.26
  def maximum (t: Tree[Int]): Int = {
    t match  {
      case Leaf(a) => a
      case Branch (l, r) => maximum(l).max(maximum(r))
    }
  }

  //Exercise 3.27
  def depth[A] (t:Tree[A]) : Int = {
    t match {
      case Leaf(_) => 1
      case Branch (l, r) => 1+ depth(l).max(depth(r))
    }
  }

  //Exercise   3.28
  def map[A, B] (t:Tree[A])(f:A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch (l, r) =>Branch (map(l)(f), map(r)(f))
    }
  }

  //Exercise 3.29
  def fold[A, B] (t:Tree[A])(f: A=> B)(g: (B, B) => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch (l, r) => g( fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def size2[A] (t: Tree[A]): Int = {
    fold(t)(a=> 1)((l, r)=> 1 + l +r )
  }


  def maximum2 (t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  def depth2[A] (t:Tree[A]) : Int = {
    fold(t)(a=> 1)((l, r)=> 1 + (l max r))
  }

  def map2[A, B] (t:Tree[A])(f:A => B): Tree[B] = {
    fold (t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
  }
}

