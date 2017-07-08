package chapter6

import scala.annotation.tailrec

/**
  * Created by couto on 5/07/17.
  */

trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }


  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    //    val (number, newRNG) = rng.nextInt
    rng.nextInt match {
      case (Integer.MIN_VALUE, newRNG) => (0, newRNG)
      case (a, newRNG) if a < 0 => (Math.abs(a), newRNG)
      case (a, newRNG) => (a, newRNG)
    }
  }

  def int(rng: RNG): (Int, RNG) = {
    rng.nextInt
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Integer.MAX_VALUE.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {

    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)


  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)

  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def loop(n: Int, acc: List[Int])(r: RNG): (List[Int], RNG) = {

      if (n <= 0) (acc, r)
      else {
        val (i, r1) = r.nextInt
        loop(n - 1, i :: acc)(r1)
      }
    }

    loop(count, Nil)(rng)
  }


  type Rand[+A] = RNG => (A, RNG)
  //val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }


  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  def doubleRand(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(i => i / (Integer.MAX_VALUE.toDouble + 1))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }


      def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
        map2(ra, rb)((_, _))
      }


      def intDoubleRand(rng: RNG): Rand[(Int, Double)] = {
        both(int, double)
      }

      def doubleIntRand(rng: RNG): Rand[(Double, Int)] = {
        both(double, int)
      }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

        fs.foldRight(unit[List[A]])()

    rng =>
    for {
      current <- fs
      (i, r) <- current(rng)
    }
  }

}