object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n * acc)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def formatResult(name: String, x: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, x, f(x))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int, sorted: Boolean): Boolean = {
      if (!sorted) false
      else if (n + 1 >= as.length) sorted
      else if (!ordered(as(n), as(n + 1))) false
      else go (n+1, sorted)
    }

    go(0, sorted = true)
  }

  def partial1[A,B,C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))

    println(formatResult("abs", -42, abs))
    println(formatResult("factorial", 7, factorial))

    println(findFirst(Array(1,2,3,4), (x: Int) => x == 2))

    println(isSorted(Array(1, 2, 3, 4, 5, 6, 7, 6), (x: Int, y: Int) => x < y))
  }
}
