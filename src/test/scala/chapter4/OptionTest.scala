package chapter4

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import chapter4.Option._
/**
  * Created by couto on 30/06/17.
  */
class OptionTest extends FlatSpec with Matchers with PropertyChecks {

  val notValid = None
  val validI = Some(5)
  val validIUnder0 = Some(-5)
  val validS = Some("hi")
  val functionI = (a: Int) => a * 5
  val functionIOption = (a: Int) => if (a > 0) Some(a * 5) else None
  val functionS = (s: String) => s.toUpperCase


  "map" should "be None for None" in {
    assert(notValid.map(functionI) == None)
    assert(notValid.map(functionS) == None)
  }

  it should "be a valid Some for Some values" in {
    assert(validI.map(functionI) == Some(25))
    assert(validS.map(_.toUpperCase) == Some("HI"))
  }

  "getOrElse" should "be the default value if None" in {
    assert(notValid.getOrElse("Nothing") == "Nothing")
  }

   it should "be the value inside Some" in {
    assert(validI.getOrElse(None) == 5)
  }

  "flatMap" should "be None for None" in {
    assert(notValid.flatMap(functionIOption) == None)
  }

  it should "be a valid Some for Some values" in {
    assert(validI.flatMap(functionIOption) == Some(25))
    assert(validIUnder0.flatMap(functionIOption) == None)
  }

  "orElse" should "be the default value if None" in {
    assert(notValid.orElse(Some("Nothing")) == Some("Nothing"))
  }

  it should "be the value inside Some" in {
    assert(validI.orElse(Some(45)) == Some(5))
  }

  "filter" should "be None if None" in {
    assert(notValid.filter((a:Int) => a > 0) == None)
  }

  it should "be the value inside Some" in {
    assert(validI.filter((a:Int) => a > 0) == Some(5))
    assert(validIUnder0.filter((a:Int) => a > 0) == None)
  }

  "variance" should "be None for empty Seq" in {
    assert(variance(Seq()) == None)
  }

}
