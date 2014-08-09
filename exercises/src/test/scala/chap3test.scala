package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "tail" should "return the tail" in {
    assert(List(2, 3) == List.tail(List(1, 2, 3)))
  }

  "setHead" should "change the head while not changing the original list" in {
    val as = List(1, 2, 3)
    assert(List(4, 2, 3) == List.setHead(as, 4))
    assert(List(1, 2, 3) == as)
  }

  "drop" should "drop the appropriate number of elements" in {
    assert(List(4, 5) == List.drop(List(1, 2, 3, 4, 5), 3))
  }

  "dropWhile" should "drop while true" in {
    assert(List(5, 1) == List.dropWhile(List(1, 2, 3, 5, 1), (x: Int) => x < 4))
  }

  "init" should "give the beginning" in {
    assert(List(1, 2, 3, 4) == List.init(List(1, 2, 3, 4, 5)))
  }

  "length" should "give the correct length" in {
    assert(3 == List.length(List(1, 2, 3)))
  }

  "reverse" should "reverse a list" in {
    assert(List(3, 2, 1) == List.reverse(List(1, 2, 3)))
  }

  "appendViaFold" should "append" in {
    assert(List(1, 2, 3, 4, 5, 6) == List.append(List(1, 2, 3), List(4, 5, 6)))
  }

  "concat" should "concat" in {
    assert(List(1, 2, 3, 4, 5, 6) == List.concat(List(List(1, 2), List(3, 4), List(5, 6))))
  }
}


