object week5{
  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 2, 3)

  def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n+1)

  removeAt(1, nums)
}
