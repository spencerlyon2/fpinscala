package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the
  // function `f` takes its second argument by name and may choose not
  // to evaluate it.

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      // If `f` doesn't evaluate its second argument, the recursion
      // never occurs.
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def toList2: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  // This one will not overflow the stack with large streams (b/c it is tco)
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc
    }

    go(this, List()).reverse
  }

  // Here `b` is the unevaluated recursive step that folds the tail of
  // the stream. If `p(a)` returns `true`, `b` will never be evaluated
  // and the computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty  // empty because either p(h()) == 0 or we have case Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAllFold (p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  def takeWhileFold(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOptionFold: Option[A] =
    this.foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  // NOTE: I got a `contravariant`issue here and needed to look up the
  // B>:A
  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  // def flatMap[B](f: A => Stream[B]): Stream[B] =
  //   foldRight(empty[B])((a, b) => cons(f(a).headOption.get, b))

  // NOTE: the above is what I wrote
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def map2[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(a, b) => Some((f(a()), b()))
      case _ => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)){
      case (Cons(a, b), x) if x > 0 => Some((a(), (b(), x-1)))
      case _ => None
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(a, b) if p(a()) => Some((a(), b()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)){
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    }

  // TODO: I don't understand what this is supposed to do...
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    empty
  }

  // TODO: Doesn't use functions I've written, but it works
  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
    case _ => false
  }

  // NOTE: check with x.tails.toList.map(_.toList)
  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case x => Some((x, x drop 1))
    } append (Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)
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

  def constant[A](a: A) : Stream [A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  val fibs = {
    def go(f1: Int, f2:Int): Stream[Int] = {
      cons(f1, go(f2, f1+f2))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  val fibs2 = unfold((0, 1)){ case (f1, f2) => Some((f1, (f2, f1+f2)))}

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s+1)))

  def constant2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s)))

  val ones2: Stream[Int] = unfold(1)(s => Some((1, 1)))

  def slinspace(a: Double, b: Double, n: Int): Stream[Double] ={
    val step = (b - a)/(n - 1)
    unfold(a){
      case x if x > b => None
      case x if x <= b => Some((x, x+step))
    }
  }

}
