package fpinscala.state

import org.scalatest._
import RNG._

class StateSpec extends FlatSpec with Matchers {
  "nonNegativeInt" should "never give negative" in {
    val (rand, rng) = nonNegativeInt(Simple(System.currentTimeMillis()))
    assert(rand > 0)
  }
  
  "nonNegativeInt" should "be repeatable with same seed" in {
    val rng: RNG = Simple(System.currentTimeMillis())
    val (r1, nextRng) = rng.nextInt
    val (r2, nextRng2) = rng.nextInt
    assert(r1 == r2)
    assert(nextRng == nextRng2)
  }
  
  "double" should "give doubles in [0,1)" in {
    val (d, rng) = double(Simple(System.currentTimeMillis()))
    assert(d >= 0.0)
    assert(d < 1.0)
  }
  
  "sequence" should "sequence" in {
    assert(List(1, 2, 3) == sequence(List(unit(1), unit(2), unit(3)))(Simple(1L))._1)
  }
}
