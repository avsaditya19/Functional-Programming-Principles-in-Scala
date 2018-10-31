object higherlist {

  /* def squareList(xs: List[Int]): List[Int] = xs match {
     case Nil => xs
     case y :: ys => (y * y) :: squareList(ys)
   }*/
  def squareList(ints: List[Int]) = ints map (x => x * x)

  squareList(List(1, 2, 3, 4))

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  pack(List("a", "a", "a", "b", "c", "c", "a"))

  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map (ys => (ys.head, ys.length))
  }

  encode(List("a", "a", "a", "b", "c", "c", "a"))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) ((x, y) => f(x) :: y)

  mapFun(List(1, 2, 3, 4), (x: Int) => x * x)

  def length[T](xs: List[T]): Int =
    (xs foldRight 0) ((x, y) => y + 1)

  length(List(1, 2, 3, 4))

}