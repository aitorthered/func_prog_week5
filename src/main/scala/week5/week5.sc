
object week5{
  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 2, 3)
  val pair = ("Answer", 42)

  def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n+1)

  removeAt(1, nums)
}

object mergesort{
  def msort(xs: List[Int]): List[Int] = {

    def merge(xs: List[Int], ys: List[Int]): List[Int] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x::xs1, y::ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(ys), msort(zs))
    }
  }
  val fruit = List("apples", "oranges", "pears")
  val nums = List(5,31,-1, 2, 3)
  
  msort(nums)

}

