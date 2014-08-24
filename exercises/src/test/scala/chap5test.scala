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

  "forAll" should "give true for true" in {
    assert(cons(1, cons(2, cons(3, Empty))).forAll(_ < 4))
  }
  "forAll" should "give false for differences" in {
    assert(!cons(1, cons(4, cons(3, Empty))).forAll(_ < 4))
  }
  
  "headOption" should "give Some for a Cons" in {
    assert(Some(1) == cons(1, cons(2, Empty)).headOption)
  }
  
  "headOption" should "give None for Empty" in {
    assert(None == Empty.headOption)
  }
  
  "map" should "map" in {
    assert(List(2, 3, 4) == cons(1, cons(2, cons(3, Empty))).map(_ + 1).toList)
  }
  "map" should "map to empty for empty" in {
    assert(Empty == (Empty: Stream[Int]).map(_ + 1))
  }
  "filter" should "filter" in {
    assert(List(1, 2) == cons(1, cons(2, cons(3, Empty))).filter(_ < 3).toList)
  }
  
  "append" should "append" in {
    assert(List(1, 2, 3, 4) == cons(1, cons(2, Empty)).append(cons(3, cons(4, Empty))).toList)
  }
  
  "flatMap" should "flatMap" in {
    assert(List(1, 1, 2, 2) == cons(1, cons(2, Empty)).flatMap(x => cons(x, cons(x, Empty))).toList)
  }
}
