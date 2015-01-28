package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max (maximum(r))
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A=>B): Tree[B] = t match{
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(x => 1)(_+_ + 1)

  def maximum2(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depth2[A](t: Tree[A]): Int = fold(t)(x => 0)((a, b) => 1 + (a max b))

  def map2[A, B](t: Tree[A])(f: A=>B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

  val t = Branch(Leaf(4), Branch(Leaf(1), Leaf(3)))



}
