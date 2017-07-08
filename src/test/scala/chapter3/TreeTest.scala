package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class TreeTest extends FlatSpec with Matchers with PropertyChecks {

  val tree =  Branch(Leaf(8), Branch(Leaf(15), Leaf(-1)))
  val x2tree =  Branch(Leaf(16), Branch(Leaf(30), Leaf(-2)))
  val boolTree =  Branch(Leaf(true), Branch(Leaf(true), Leaf(false)))

  "size" should "be 1 for Leaf" in {
    assert(Tree.size(Leaf(5)) == 1)
    assert(Tree.size(Leaf(5)) == Tree.size2(Leaf(5)))
  }

  it should "work  properly" in {
    assert(Tree.size(tree) == 5)
    assert(Tree.size(tree) == Tree.size2(tree))
  }

  "maximum" should "work  properly" in {
    assert(Tree.maximum(tree) == 15)
    assert(Tree.maximum(tree) == Tree.maximum2(tree))
  }

  "depth" should "work  properly" in {
    assert(Tree.depth(tree) == 3)
    assert(Tree.depth(tree) == Tree.depth2(tree))
  }

  "map" should "work  properly" in {
    assert(Tree.map(tree)(_*2) == x2tree)
    assert(Tree.map(tree)(_>0) == boolTree)
    assert(Tree.map2(tree)(_*2) == x2tree)
    assert(Tree.map2(tree)(_>0) == boolTree)
  }



}

