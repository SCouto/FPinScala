package chapter4

/**
  * Created by couto on 30/06/17.
  */
sealed trait Either[+E, +A] {


  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

  def flatMap [EE >: E, B >: A](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  }


  def orElse [EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) :
  Either[EE, C] = {
    (this, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }
//    for {
//      aa: A <- this
//      bb: B <- b
//    } yield  f(aa, bb)
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {

  def sequenceold[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    a match {
      case Nil => Right(Nil)
      case h::t => h.map2(sequenceold(t))(_::_)
    }
  }

  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(a)(x => x)
  }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    a match {
      case Nil => Right(Nil)
      case h::t => f(h).map2(traverse(t)(f))(_::_)
    }
  }
}
