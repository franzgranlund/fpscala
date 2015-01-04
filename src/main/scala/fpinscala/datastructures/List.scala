package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def loop(i: Int, xs: List[A]): List[A] = {
      if (i == n) xs
      else loop(i + 1, tail(xs))
    }

    if (l == Nil) sys.error("drop on empty list")
    else loop(0, l)
  }

  def drop2[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop2(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else t
    }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) =>  Cons(h, init(t))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  def reverse[A](ns: List[A]): List[A] = foldLeft(ns, Nil:List[A])((a, x) => Cons(x, a))

  def foldRightViaFoldLeft[A,B](ns: List[A], z: B)(f: (A,B) => B): B = {
    val f1 = (b: B, a: A) => f(a, b)
    foldLeft(reverse(ns), z)(f1)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((acc, h) => Cons(h, acc))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil:List[A])((acc, xs) => append(acc, xs))
  }

  def add(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, acc) => Cons(h + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h, acc) => Cons(h.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((h, acc) => Cons(f(h), acc))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    //foldRight(as, Nil:List[B])((h, acc) => append(f(h), acc))
    concat(map(as)(f))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def loop(sup1: List[A], sub1: List[A], m: Int): Int = {
      (sup1, sub1) match {
        case (Nil, Nil) => m
        case (Nil, Cons(h, t)) => 0
        case (_, Nil) => m
        case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) loop(t1, t2, m + 1) else loop(t1, sub1, 0)
      }
    }

    loop(sup, sub, 0) == length(sub)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}