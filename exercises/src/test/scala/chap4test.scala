package fpinscala.errorhandling

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {
  "map" should "give some for some" in {
    assert(Some(2) == Some(1).map(_ + 1))
  }
  "map" should "give none for none" in {
    val n: Option[Int] = None
    assert(None == n.map(_ + 1))
  }

  "getOrElse" should "get for some" in {
    assert(2 == Some(2).getOrElse(3))
  }

  "getOrElse" should "else for none" in {
    assert(3 == None.getOrElse(3))
  }

  "flatMap" should "flatten" in {
    assert(Some(3) == Some(2).flatMap(x => Some(x + 1)))
  }

  "orElse" should "give first on some" in {
    assert(Some(1) == Some(1).orElse(Some(2)))
  }
  "orElse" should "give second on none" in {
    val n: Option[Int] = None
    assert(Some(2) == (n orElse Some(2)))
  }
  "filter" should "give some on true" in {
    assert(Some(1) == (Some(1).filter(x => true)))
  }
  "filter" should "give none on false" in {
    assert(None == (Some(1).filter(x => false)))
  }
  "filter" should "give none on None" in {
    assert(None == (None.filter(x => false)))
  }

  "sequence" should "give some when the list contains only some" in {
    assert(Some(List(1, 2, 3, 4)) == Option.sequence(List(Some(1), Some(2), Some(3), Some(4))))
  }
  "sequence" should "give none when the list contains at least one none" in {
    assert(None == Option.sequence(List(Some(1), None, Some(3), Some(4))))
  }


}
