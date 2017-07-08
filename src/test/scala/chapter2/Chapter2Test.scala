package chapter2

import chapter2.Chapter2._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class Chapter2Test extends FlatSpec with Matchers with PropertyChecks {


  import org.scalacheck.Gen

  val genPositiveInteger = for (n <- Gen.choose(0, 500)) yield n
  val genInteger = for (n <- Gen.choose(-500, 500)) yield n
  val genIntArray = Gen.containerOf[Array, Int](genInteger)
  val genStringArray = Gen.containerOf[Array, String](Gen.alphaStr)
  val genBoolArray = Gen.containerOf[Array, Boolean](Gen.oneOf(true, false))


  "Fib" should "be equals to the addition of the two previous fib or n if n < 2" in {

    forAll((genPositiveInteger, "testNumber")) { (n: Int) =>
      whenever(n >= 0) {

        val result = fib(n)
        if (n <= 1) result shouldEqual n
        else result shouldEqual fib(n - 1) + fib(n - 2)
      }
    }
  }

  "isSorted" should "return true for empty arrays" in {
    val aInt: Array[Int] = Array()
    val aString: Array[String] = Array()
    val aBoolean: Array[Boolean] = Array()

    isSorted[Int](aInt, intOrdering) shouldEqual true
    isSorted[String](aString, stringOrdering) shouldEqual true
    isSorted[Boolean](aBoolean, booleanOrdering) shouldEqual true

  }

  it should "return true for one-element arrays" in {
    val aInt: Array[Int] = Array(1)
    val aString: Array[String] = Array("myName")
    val aBoolean: Array[Boolean] = Array(true)

    isSorted[Int](aInt, intOrdering) shouldEqual true
    isSorted[String](aString, stringOrdering) shouldEqual true
    isSorted[Boolean](aBoolean, booleanOrdering) shouldEqual true

  }

  it should "return false for unsorted arrays and true for the same sorted arrays" in {

    val aInt: Array[Int] = Array(2, 1)
    val aString: Array[String] = Array("yourName", "myName")
    val aBoolean: Array[Boolean] = Array(true, false)

    isSorted[Int](aInt, intOrdering) shouldEqual false
    isSorted[String](aString, stringOrdering) shouldEqual false
    isSorted[Boolean](aBoolean, booleanOrdering) shouldEqual false

    isSorted[Int](aInt.sorted, intOrdering) shouldEqual true
    isSorted[String](aString.sorted, stringOrdering) shouldEqual true
    isSorted[Boolean](aBoolean.sorted, booleanOrdering) shouldEqual true

  }

  it should "return true for any sorted arrays" in {

    forAll((genIntArray, "testArray")) { (myArray: Array[Int]) =>
      isSorted[Int](myArray.sorted, intOrdering) shouldEqual true

    }

    forAll((genStringArray, "testArray")) { (myArray: Array[String]) =>
      isSorted[String](myArray.sorted, stringOrdering) shouldEqual true

    }

    forAll((genBoolArray, "testArray")) { (myArray: Array[Boolean]) =>
      isSorted[Boolean](myArray.sorted, booleanOrdering) shouldEqual true
    }


  }
}

