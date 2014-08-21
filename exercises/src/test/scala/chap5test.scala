package fpinscala.laziness

import org.scalatest._
import Stream._

class StreamSpec extends FlatSpec with Matchers {
  "toList" should "turn a stream into a list" in {
    assert(List(1, 2, 3) == cons(1, cons(2, cons(3, Empty))).toList)
  }

  "take" should "take the first n elements" in {
    assert(List(1, 2) == cons(1, cons(2, cons(3, Empty))).take(2).toList)
  }

  "drop" should "drop the first n elements" in {
    assert(List(3, 4) == cons(1, cons(2, cons(3, cons(4, Empty)))).drop(2).toList)
  }

  "takeWhile" should "take while p is true" in {
    assert(List(1, 2, 3) == cons(1, cons(2, cons(3, cons(4, Empty)))).takeWhile(_ < 4).toList)
  }
}
