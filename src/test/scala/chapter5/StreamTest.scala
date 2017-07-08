package chapter5

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by couto on 30/06/17.
  */
class StreamTest extends FlatSpec with Matchers with PropertyChecks {

    val s = chapter5.Stream(1,2,3)

  "startsWith" should "return true for empty Streams" in {
    assert(s.startsWith(Empty))
    assert(s.startsWith(Stream(1)))
    assert(s.startsWith(Stream(1, 2)))
    assert(!s.startsWith(Stream(1, 3)))
  }


}
