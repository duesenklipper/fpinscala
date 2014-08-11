package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))


  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + l + r)

  def maximum(t: Tree[Int]): Int =
    fold(t)(x => x)((l, r) => l max r)


  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
    case Leaf(x) => l(x)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }
}