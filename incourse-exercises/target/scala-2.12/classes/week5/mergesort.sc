
object mergesort {
  /*
  //Without Ordering
  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
      val n = xs.length / 2
      if (n == 0) xs
      else {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) =>
            if (lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

        val (fst, snd) = xs splitAt n
        merge(msort(fst)(lt), msort(snd)(lt))
      }
    }


    msort(List(5, -5, 6, 1, 3, 0, 87, 9))((x, y) => x < y) //type inferred
    msort(List("apple", "banana", "orange", "baaaaaaaaa"))((x: String, y: String) => x.compareTo(y) < 0)
    */

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd)) // you can leave out ord with implicit
    }
  }

  msort(List(5, -5, 6, 1, 3, 0, 87, 9)) //implicit
  msort(List("apple", "banana", "orange", "baaaaaaaaa"))
}