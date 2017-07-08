package chapter6

import chapter6.RNG.SimpleRNG
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import RNG._
/**
  * Created by couto on 6/07/17.
  */
class RNGTest extends FlatSpec with Matchers with PropertyChecks {


  val simpleRNG = SimpleRNG(42)

  "ints" should "return empty list if n <= 0" in {
      assert(ints(0)(simpleRNG)._1.isEmpty)
      assert(ints(-5)(simpleRNG)._1.isEmpty)
  }
  it should "return a proper list i n >0" in {
    val (l, r) = ints(5)(simpleRNG)
    l.foreach(println)
    assert(l.size == 5)
  }

  }
