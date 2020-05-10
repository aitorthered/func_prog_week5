
object week5 {
  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 2, 3)
  val pair = ("Answer", 42)

  def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1)

  removeAt(1, nums)
}

object mergesort {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {

    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(ys), msort(zs))
    }
  }

  val fruit = List("oranges", "apples", "pears")
  val nums = List(5, 31, -1, 2, 3)

  msort(nums)
  msort(fruit)

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }

  def squareListMap(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  squareList(nums)
  squareListMap(nums)

  nums filter (x => x > 0)
  nums filterNot (x => x > 0)
  nums partition (x => x > 0)

  nums takeWhile (x => x > 0)
  nums dropWhile (x => x > 0)
  nums span (x => x > 0)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (z => z == x)
      first :: pack(rest)
  }

  pack(nums)
  pack(List("a", "a", "a", "b", "c", "c", "a"))

  def encode[T](xs: List[T]): List[(T,Int)] =
    pack(xs) map( list => (list.head, list.length) )

  encode(List("a", "a", "a", "b", "c", "c", "a"))
  //List(("a", 3), ("b", 1), ("c", 2), ("a", 1))

}
