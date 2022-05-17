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
//  println(datesInMonths(List((1,2,2222),(1,2,2222),(1,4,2222),(1,2,2222),(1,2,2222),(1,5,2222)),List(1,2,4)))

  def dateToString(d: Date): String = {
    val listOfMonths = List("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    listOfMonths(d._2 - 1) + "-" + d._1 + "-" + d._3
  }
//  println(dateToString((10,5,2002)))

  def whatMonth(n: Int, yr: Int): Int = {
    val numberOfDaysInMonth = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    val leapYearMonth = List(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    @tailrec
    def whatMonthHelper(month: List[Int], number: Int, answer: Int): Int = month match {
      case Nil => answer
      case head :: tail => if (number > head) whatMonthHelper(tail, number - head, answer + 1)
      else whatMonthHelper(tail, number - head, answer)
    }
    if (yr%4 == 0)
    whatMonthHelper(leapYearMonth, n, 1)
    else whatMonthHelper(numberOfDaysInMonth, n, 1)
  }
//  println(whatMonth(60,2011))

  def oldest(dates: List[Date]): Option[Date] = {
    if (dates.isEmpty) None
    else {
      def oldestHelper(h: Date, source: List[Date]): Date = source match {
        case Nil => h
        case head :: tail =>
          if (isOlder(h, head)) {
            oldestHelper(head, tail)
          }
          else oldestHelper(h, tail)
      }
      Some(oldestHelper(dates.head, dates.tail))
    }
  }
//  println(oldest(List((1,2,2222),(1,5,2222),(1,3,2222),(1,12,2222),(1,9,2224))))

  def isReasonableDate(d: Date): Boolean = {
    val day = d._1
    val month = d._2
    val year = d._3
    if (year > 0 && month > 0 && month < 13 && day >0 && day < 32) {
      def getDaysInMonth(month: Int, year: Int): Int = {
        if (month == 2 && (year%400 == 0 || year%4 ==0) && year%100 != 0) 29
        else {
          val month_sums = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
          month_sums(d._2 - 1)
        }
      }
      if (getDaysInMonth(month, year) < day) false
      else true
    }
    else false
  }
//  println(isReasonableDate(29,2,2011))
}
