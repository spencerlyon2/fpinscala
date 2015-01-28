package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
  if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
  a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]) = foldRight(l, 0)((x, y) => x+y)

  def product2(l: List[Double]) = foldRight(l, 1.0)(_*_)

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_ ,x: Int) => x +1)

  // Exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11
  def sumLeft(l: List[Int]) = foldLeft(l, 0)(_+_)

  def productLeft(l: List[Double]) = foldLeft(l, 1.0)(_*_)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x +1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((a,b)=>Cons(b, a))

  // Exercise 3.13
  def foldRightFromLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  // Exercise 3.14
  def appendFoldR[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  // Exercise 3.15
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(appendFoldR(_, _))

  // Exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil: List[B]
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // Exercise 3.16
  def addOneInt(l: List[Int]) = map(l)(_+1)

  // Exercise 3.17
  def convertToString(l: List[Double]) = map(l)(x=>x.toString)

  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightFromLeft(l, Nil: List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  // Exercise 3.20
  def flatMap[A, B](l: List[A])(f: A=> List[B]): List[B] = concat(map(l)(f))

  // Exercise 3.21
  def filterFromFlatMap[A](l: List[A])(f: A => Boolean) =
    flatMap(l)(a=> if (f(a)) Cons(a, Nil) else Nil)

  // Exercise 3.22
  def addLists(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, y) => Nil
    case (x, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
  }

  // Exercise 3.23
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
  (l, r) match {
    case (Nil, y) => Nil
    case (x, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
  }

  def contains[A](l: List[A], x: A): Boolean = (l, x) match {
    case (Nil, _) => false
    case (Cons(y, ys), x) => if (y == x) true else contains(ys, x)
  }

  // Exercise 3.24 TOOD: This isn't correct
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean = (a, b) match {
    case (Nil, Cons(_, _)) => false
      case (_, Nil) => true  // all Lists end with Cons(_, Nil)
      case (Cons(x, xs), Cons(y, ys)) =>
      if (x == y) hasSubsequence(xs, ys)
      else hasSubsequence(xs, b)
    }
}
