package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import List._


class ListTest extends FlatSpec with Matchers with PropertyChecks {


  "x" should "be 3" in {
    assert(x == 3)
  }


  "tail" should "be empty for empty lists" in {
    val l = List()
    assert(tail(l) == Nil)
  }

  it should "be empty for one-element lists" in {
    val l = List(1)
    assert(tail(l) == Nil)
  }

  it should "be the tail itself for bigger lists" in {

    val lInt = List(1, 2, 5, 7)
    assert(tail(lInt) == List(2, 5, 7))

    val lString = List("myName", "mySurname")
    assert(tail(lString) == List("mySurname"))

    val lBool = List(true, true, false)
    assert(tail(lBool) == List(true, false))

  }


  "setHead" should "be an one-element list for empty lists" in {
    val l = List()
    assert(setHead(1, l) == List(1))
  }


  it should "be the setHead itself for bigger lists" in {

    val lInt = List(1, 2, 5, 7)
    assert(setHead(0, lInt) == List(0, 2, 5, 7))

    val lString = List("myName", "mySurname")
    assert(setHead("mr", lString) == List("mr", "mySurname"))

    val lBool = List(true, true, false)
    assert(setHead(false, lBool) == List(false, true, false))

  }

  "drop" should "be empty for empty lists" in {
    val l = List()
    assert(drop(l, 5) == Nil)
    assert(drop(l, 0) == Nil)
    assert(drop(l, -5) == Nil)
  }

  it should "be empty for one-element lists" in {
    val l = List(1)
    assert(drop(l, 5) == Nil)
  }

  it should "be the same list for 0 drops regardless the list itself" in {
    val l = List(1)
    val ls = List("myName", "mySurname")
    assert(drop(l, 0) == l)
    assert(drop(Nil, 0) == Nil)
    assert(drop(ls, 0) == ls)
  }

  it should "be the list without the x elements at the beginning for bigger lists" in {

    val lInt = List(1, 2, 5, 7)
    assert(drop(lInt, 3) == List(7))
    assert(drop(lInt, 4) == Nil)
    assert(drop(lInt, 1) == tail(lInt))
    assert(drop(lInt, 15) == Nil)

  }


  "dropWhileold" should "be empty for empty lists" in {
    val l = List()
    assert(dropWhileold(l, (x: Int) => x > 5) == Nil)
    assert(dropWhileold(l, (x: Int) => x > 5) == Nil)
    assert(dropWhileold(l, (x: Int) => x > 5) == Nil)
  }

  it should "work as expected" in {

    val lInt = List(1, 2, 5, 7)
    assert(dropWhileold(lInt, (x: Int) => x < 5) == List(5, 7))
    assert(dropWhileold(lInt, (x: Int) => x < 10) == Nil)
    assert(dropWhileold(lInt, (x: Int) => true) == Nil)
    assert(dropWhileold(lInt, (x: Int) => false) == List(1, 2, 5, 7))

  }

  "dropWhile" should "be empty for empty lists" in {
    //cannot infer type from empty list, we should
    val l: List[Int] = List()
    assert(dropWhile(l)(x => x > 5) == Nil)
    assert(dropWhile(l)(x => x > 5) == Nil)
    assert(dropWhile(l)(x => x > 5) == Nil)
  }

  it should "work as expected" in {

    val lInt = List(1, 2, 5, 7)
    assert(dropWhile(lInt)(x => x < 5) == List(5, 7))
    assert(dropWhile(lInt)(x => x < 10) == Nil)
    assert(dropWhile(lInt)(x => true) == Nil)
    assert(dropWhile(lInt)(x => false) == List(1, 2, 5, 7))

  }

  "init" should "be empty for empty lists" in {
    val l = List()
    assert(init(l) == Nil)
  }

  it should "be empty for one-element lists" in {
    val l = List(1)
    assert(init(l) == Nil)
  }


  it should "work properly for bigger lists" in {
    val l = List(1, 2, 5, 1)
    assert(init(l) == List(1, 2, 5))
  }

  "foldRight" should "be checked for exercise 3.8" in {
    val result = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    assert(result == List(1, 2, 3))
  }

  "length" should "be 0 for empty lists" in {
    val l = List()
    assert(List.length(l) == 0)
  }

  it should "work properly for bigger lists" in {
    assert(List.length(List(1, 2, 5, 1)) == 4)
    assert(List.length(List(6)) == 1)
  }

  "foldLeft" should "work properly" in {

    val list = List(1, 2, 3, 4, 5)
    val result = foldLeft(list, 0)(_ + _)
    assert(result == sum(list))
    assert(result == sum2(list))

    val resultp = foldLeft(list, 1)(_ * _)
    assert(resultp == product(list))
    assert(resultp == product2(list))
  }

  "lengthLeft" should "be 0 for empty lists" in {
    val l = List()
    assert(List.lengthLeft(l) == 0)
  }

  it should "work properly for bigger lists" in {
    assert(List.lengthLeft(List(1, 2, 5, 1)) == 4)
    assert(List.lengthLeft(List(6)) == 1)
  }

  "sumLeft" should "work properly" in {
    val list = List(1, 2, 3, 4, 5)
    val result = sumLeft(list)
    assert(result == sum(list))
    assert(result == sum2(list))
    assert(sumLeft(Nil) == 0)
  }

  "productLeft" should "work properly" in {
    val list = List(1, 2, 3, 4, 5)
    val resultp = productLeft(list)
    assert(resultp == product(list))
    assert(resultp == product2(list))
    assert(productLeft(Nil) == 1.0)
  }

  "reverse" should "return empty for empty lists" in {
    assert(reverse(Nil) == Nil)
  }

  it should "work properly" in {
    val list = List(1, 2, 3, 4, 5)
    assert(reverse(list) == List(5, 4, 3, 2, 1))
  }

  "foldLeftWithFoldRight" should "work properly" in {
    val list = List(1, 2, 3, 4, 5)
    val result = foldLeftWithFoldRight(list, 0)(_ + _)
    assert(result == sum(list))
    assert(result == sum2(list))

    val resultp = foldLeftWithFoldRight(list, 1)(_ * _)
    assert(resultp == product(list))
    assert(resultp == product2(list))


    val resultl = foldLeftWithFoldRight(list, 0)((x, y) => x + 1)
    assert(resultl == 5)
  }

  "foldRightWithFoldLeft" should "work properly" in {
    val list = List(1, 2, 3, 4, 5)
    val result = foldRightWithFoldLeft(list, 0)(_ + _)
    assert(result == sum(list))
    assert(result == sum2(list))

    val resultp = foldRightWithFoldLeft(list, 1)(_ * _)
    assert(resultp == product(list))
    assert(resultp == product2(list))


    val resultl = foldRightWithFoldLeft(list, 0)((x, y) => y + 1)
    assert(resultl == 5)
  }

  "appendfold" should "be empty for empty lists" in {
    assert(appendfoldRight(List(), List()) == Nil)
    assert(appendfoldLeft(List(), List()) == Nil)
  }

  it should "be empty the non-empty list if one of them is empty" in {
    assert(appendfoldRight(List(1, 2, 3), List()) == List(1, 2, 3))
    assert(appendfoldLeft(List("name"), List()) == List("name"))


    assert(appendfoldRight(List(), List(1, 2, 3)) == List(1, 2, 3))
    assert(appendfoldLeft(List(), List("name")) == List("name"))
  }

  it should "be the appended list if both list are non-empty" in {
    assert(appendfoldRight(List(1, 2), List(4, 5, 6)) == List(1, 2, 4, 5, 6))
    assert(appendfoldLeft(List("name", "surname"), List("alias")) == List("name", "surname", "alias"))
  }

  "appendlists" should "be empty for empty lists" in {
    assert(appendLists(List()) == Nil)
  }

  it should "be a list containing all the elements otherwise" in {
    assert(appendLists(List(List(1, 2, 3), List(4), List(5, 6, 7))) == List(1, 2, 3, 4, 5, 6, 7))
    assert(appendLists(List(List(1, 2, 3), Nil, List(4), List(5, 6, 7))) == List(1, 2, 3, 4, 5, 6, 7))
  }

  "addOne" should "be empty for empty lists" in {
    assert(addOne(List()) == Nil)
  }

  it should "be a list containing all the elements otherwise" in {
    assert(addOne(List(1, 2, 3)) == List(2, 3, 4))
  }

  "doubleToString" should "be empty for empty lists" in {
    assert(doubleToString(List()) == Nil)
  }

  it should "be a list containing all the elements otherwise" in {
    assert(doubleToString(List(1, 2, 3.5)) == List("1.0", "2.0", "3.5"))
  }

  "map" should "be empty for empty lists" in {
    assert(map(List[Int]())(a => a + 1) == Nil)
  }

  it should "be the same as addOne" in {
    val l = List(1, 2, 3)
    assert(map(l)(a => a + 1) == addOne(l))
  }

  it should "be the same as doubleToString" in {
    val l = List(1.1, 2, 3)
    assert(map(l)(_.toString) == doubleToString(l))
  }

  "filter" should "do this" in {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val res = filter(l)(a => a % 2 == 0)
    assert(res == List(2, 4, 6, 8, 10))
  }

  "flatMap" should "do this" in {
    val l = List(1, 2, 3)
    assert(flatMap(l)(a => List(a, a)) == List(1, 1, 2, 2, 3, 3))
  }

  "filterWithFlatMap" should "do this" in {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val f = (a: Int) => a % 2 == 0
    assert(filterWithFlatMap(l)(f) == filter(l)(f))
  }

  "addLists" should "do this" in {
    val l1 = List(1, 3, 6, 7, 10)
    val l2 = List(1, 2, 3, 4, 5)
    assert(addLists(l1, l2) == List(2, 5, 9, 11, 15))
  }

  "zipWith" should "do this" in {
    val l1 = List(1, 3, 6, 7, 10)
    val l2 = List(1, 2, 3, 4, 5)
    assert(addLists(l1, l2) == zipWith(l1, l2)((a, b) => a+b))
    assert(List(0, 1, 3, 3, 5) == zipWith(l1, l2)((a, b) => a - b))
  }

  "hasSubsequence" should "do this" in {
    val l1 = List(1, 2,3, 4)
    assert(hasSubsequence(l1, List(1)))
    assert(hasSubsequence(l1, List(1, 2)))
    assert(hasSubsequence(l1, List(2)))
    assert(hasSubsequence(l1, List(2, 3)))
    assert(hasSubsequence(l1, List(4)))
    assert(hasSubsequence(l1, Nil))
    assert(hasSubsequence(Nil, Nil))
    assert(!hasSubsequence(Nil, List(1)))
  }



}

