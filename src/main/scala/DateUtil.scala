import scala.annotation.tailrec

object DateUtil extends App {
  type Date = (Int, Int, Int)

  def isOlder(x: Date, y: Date): Boolean = {
    if (x._3 < y._3) true
      else if (x._3 == y._3 && x._2 < y._2) true
      else if (x._3 == y._3 && x._2 == y._2 && x._1 < y._1) true
    else false
  }
//  println(isOlder((10,4,2002),(10,4,2002)))

  def numberInMonth(xs: List[Date], month: Int): Int = {
    @tailrec
    def numberInMonthHelper(source: List[Date], answer: Int): Int = source match {
      case Nil => answer
      case head :: tail => if (head._2 == month) numberInMonthHelper(tail, answer + 1)
      else numberInMonthHelper(tail, answer)
    }
    numberInMonthHelper(xs, 0)
  }
//  println(numberInMonth(List((1,2,2222),(1,2,2222),(1,4,2222),(1,2,2222),(1,2,2222),(1,5,2222)),2))


  def numberInMonths(xs: List[Date], months: List[Int]): Int = {
    @tailrec
    def numberInMonthsHelper(m: List[Int], answer: Int): Int = m match {
      case Nil => answer
      case head :: tail => numberInMonthsHelper(tail, answer + numberInMonth(xs, head))
    }
    numberInMonthsHelper(months, 0)
  }
//  println(numberInMonths(List((1,2,2222),(1,2,2222),(1,4,2222),(1,2,2222),(10,2,2222),(1,5,2222)),List(1,2,3,4)))

  def datesInMonth(xs: List[Date], month: Int): List[Date] = {
    @tailrec
    def datesInMonthHelper(source: List[Date], answer: List[Date]): List[Date] = source match {
      case Nil => answer
      case head :: tail => if (head._2 == month) datesInMonthHelper(tail, answer:+head)
      else datesInMonthHelper(tail, answer)
    }
    datesInMonthHelper(xs, List())
  }
//  println(datesInMonth(List((1,2,2222),(1,2,2222),(1,4,2222),(1,2,2222),(1,2,2222),(1,5,2222)),2))

  def datesInMonths(xs: List[Date], months: List[Int]): List[Date] = {
    @tailrec
    def datesInMonthsHelper(m: List[Int], answer: List[Date]): List[Date] = m match {
      case Nil => answer
      case head :: tail => datesInMonthsHelper(tail, answer ++ datesInMonth(xs, head))
    }
    datesInMonthsHelper(months, List())
  }
  println(datesInMonths(List((1,2,2222),(1,2,2222),(1,4,2222),(1,2,2222),(1,2,2222),(1,5,2222)),List(1,2,4)))

  def dateToString(d: Date): String = {
    val listOfMonths = List("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

  }

  def whatMonth(n: Int, yr: Int): Int = ???

  def oldest(dates: List[Date]): Option[Date] = ???

  def isReasonableDate(d: Date): Boolean = ???
}
