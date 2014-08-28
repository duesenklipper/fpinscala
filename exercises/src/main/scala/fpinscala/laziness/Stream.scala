package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  // z ist das eventuell ende des streams, wenn es bis ganz zum schluss weitergeht
  // wenn aber die funktion vorher schon aufhoert, ist schon frueher schluss und z wird
  // nie ausgewertet
  // foldRight heisst hier also nicht, dass ganz rechts "angefangen" wird. wir kommen von links und die funktion
  // selbst entscheidet ob weiter nach rechts gelaufen wird, das z wird aber rechts angehaengt.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = take_unfold(n)

  def take_unfold(n: Int): Stream[A] = unfold((n, this)) {
    case (n, s) => s match {
      case Cons(h, t) if n > 0 => Some((h(), (n - 1, t())))
      case _ => None
    }
  }
  
  def take_direct(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) Cons(() => h(), () => t().take(n - 1)) else Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 0) this else t().drop(n - 1)
  }

  def takeWhile_unfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def takeWhile_foldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)
  def takeWhile(p: A => Boolean): Stream[A] = takeWhile_unfold(p)

  def takeWhile_classic(p: A => Boolean): Stream[A] = this  match {
    case Empty => Empty
    case Cons(h, t) => {
      lazy val head = h()
      if (p(head)) Cons(() => head, () => t().takeWhile(p))
      else Empty
    }
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def toListSlow: List[A] = this.foldRight(Nil: List[A])((a, list) => a :: list)
  def toList: List[A] = {
    val buf = new ListBuffer[A]
    @tailrec def loop(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) => {
        buf += h()
        loop(t())
      }
    }
    loop(this)
  }
  
  def headOption: Option[A] = foldRight[Option[A]](None)((h, t) => Some(h))
  
  def map_foldRight[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((h, t) => cons(f(h), t))
  def map_unfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }
  def map[B](f: A => B): Stream[B] = map_unfold(f)
  
  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)
  
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((h, t) => f(h) append t)
  
  def zipWith[B, C](s: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this, s)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(),h2()), (t1(),t2())))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]), (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())), (empty[A], t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
    }

  // type inference problem?
  //    unfold((this, s2)) {
//      case (Cons(h, t), Empty) => Some((Some(h()), None: Option[B]), (t(): Stream[A], Empty: Stream[B]))
//      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
//      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
//      case (Empty, Empty) => None
//    }

  def startsWith_mine[A](s: Stream[A]): Boolean = zipWithAll(s){
    case (Some(a1), Some(a2)) => a1 == a2
    case (_, None) => true
    case (None, _) => false
  }.forAll(_ == true)

  def startsWith[A](s: Stream[A]): Boolean = startsWith_book(s)

  def startsWith_book[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case x: Cons[A] => Some((x: Stream[A], x.t()))
    case Empty => None
  } append Stream(Empty)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = unfold(1)(x => Some(1, 1))
  
  def constant[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))
  
  def from(n: Int): Stream[Int] = unfold(n)((s) => Some((n, n + 1)))
  
  def fibs(): Stream[Int] = {
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}