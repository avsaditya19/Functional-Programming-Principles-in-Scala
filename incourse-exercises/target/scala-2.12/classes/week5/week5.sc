
object week5 {
  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 2, 3)
  val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
  val empty = List()

  def removeAt[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop n + 1)

  removeAt(nums, 1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case (y :: ys) :: yss => flatten(y :: ys) ::: flatten(yss)
    case y :: ys => y :: flatten(ys)
  }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))

}
