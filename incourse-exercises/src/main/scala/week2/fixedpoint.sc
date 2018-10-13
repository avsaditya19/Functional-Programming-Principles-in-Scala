import math.abs

object exercise {
  var tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double): Boolean =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      println("guess =" + guess)
      val next = f(guess)
      println("next =" + next)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x / 2)(1)

  //Doesn't Converge
  //def sqrt(x: Double): Double = fixedPoint(y => x / y)(1)
  //Average Damped:
  //def sqrt(x: Double): Double = fixedPoint(y => (y + x / y) / 2)(1)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double): Double = fixedPoint(averageDamp(y => x / y))(1)

  sqrt(2)
}