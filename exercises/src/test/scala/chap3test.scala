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

  "add1" should "add 1 to each elem" in {
    assert(List(2, 3, 4) == List.add1(List(1, 2, 3)))
  }

  "toStringList" should "convert every element into a string" in {
    assert(List("1.0", "2.0", "3.0") == List.toStringList(List(1.0, 2.0, 3.0)))
  }

  "filter" should "filter" in {
    assert(List(2, 4, 6) == List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
  }

  "flatmap" should "flatmap" in {
    assert(List(1, 1, 2, 2, 3, 3) == List.flatMap(List(1, 2, 3))(x => List(x, x)))
  }

  "addElems" should "add" in {
    assert(List(3, 5, 7) == List.addElems(List(1, 2, 3), List(2, 3, 4)))
  }

  "hasSubsequence" should "find a subseq that exists" in {
    assert(List.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)))
  }
  "hasSubsequence" should "not find a subseq that does not exist" in {
    assert(!List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 3, 4)))
  }


}

class TreeSpec extends FlatSpec with Matchers {
  "size" should "size the tree correctly" in {
    assert(5 == Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
  }

  "maximum" should "find the maximum" in {
    assert(3 == Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
  }

  "depth" should "find the depth" in {
    assert(3 == Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))))
  }

  "map" should "map" in {
    val expected: Branch[Int] = Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))
    val initial: Branch[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

    assert(expected == Tree.map(initial)(_ + 1))
  }
}


