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
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) Cons(() => h(), () => t().take(n - 1)) else Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 0) this else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def takeWhile_classic(p: A => Boolean): Stream[A] = this  match {
    case Empty => Empty
    case Cons(h, t) => {
      lazy val head = h()
      if (p(head)) Cons(() => head, () => t().takeWhile(p))
      else Empty
    }
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def startsWith[A](s: Stream[A]): Boolean = sys.error("todo")

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
  
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((h, t) => cons(f(h), t))
  
  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)
  
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((h, t) => f(h) append t)
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

  val ones: Stream[Int] = Stream.cons(1, ones)
  
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
