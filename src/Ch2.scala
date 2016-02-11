/**
  * Created by jimin on 2/9/16.
  */
object Ch2 {

  /**
    * EXERCISE 2.1
    *
    * @param n
    * @return
    */
  def fib(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, current: Int, previous: Int): Int = {
      if (n == 0) previous // When n = 0, there is nothing to do.
      else go(n - 1, current + previous, current)
    }

    go(n, 1, 0)
  }

  /**
    * EXERCISE 2.2
    *
    * @param as
    * @param ordered
    * @tparam A
    * @return
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * EXERCISE 2.3
    * converts a function f of two arguments into a function of one argument that partially applies f.
    *
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
    * EXERCISE 2.4
    *
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)


  /**
    * EXERCISE 2.5
    * Implement the higher-order function that composes two functions.
    *
    * @param f
    * @param g
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }


  def main(args: Array[String]) {
    println(fib(10))
  }
}
