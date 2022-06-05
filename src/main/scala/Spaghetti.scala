object Spaghetti extends App {
  def spaghetti: Stream[String] = {
    def spaghettiHelper(previous: List[String]): Stream[List[String]] = {
      def next(num: List[String]): List[String] = num match {
        case Nil => Nil
        case head :: Nil => "1" :: head :: Nil
        case head :: _ =>
          val size = (num takeWhile (_.equals(head))).size
          List(size.toString, head) ::: next(num.drop(size))
      }
      val x = next(previous)
      x #:: spaghettiHelper(x)
    }
    "1" #:: spaghettiHelper("1" :: Nil).map(_.mkString(""))
  }

  def ham: LazyList[String] = {
    def hamHelper(s: LazyList[String]): LazyList[String] = {
      val newStream = s.flatMap(h => LazyList("0", "1").map(h + _))
      newStream #::: hamHelper(newStream)
    }
    LazyList("0", "1") #::: hamHelper(LazyList("0", "1"))
  }

}
