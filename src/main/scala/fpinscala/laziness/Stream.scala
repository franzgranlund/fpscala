package fpinscala.laziness

// val x = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Empty))))

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => Stream.cons(h(), Empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def toListRecursive: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }
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
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}