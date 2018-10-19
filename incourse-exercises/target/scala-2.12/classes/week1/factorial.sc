import scala.annotation.tailrec

object factorial {

  def factorial(n: Int): Int = {
    @tailrec
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(n * acc, n - 1)

    loop(1, n)
  }

  factorial(4)
}