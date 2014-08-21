package fpinscala.laziness

import org.scalatest._
import Stream._

class StreamSpec extends FlatSpec with Matchers {
  "toList" should "turn a stream into a list" in {
    assert(List(1, 2, 3) == cons(1, cons(2, cons(3, Empty))).toList)
  }
}
