object UseLib extends App {
  def onlyBeginsWithLower(xs: Vector[String]): Vector[String] = {
    xs.filter(_.charAt(0).isLower)
  }

  def longestString(xs: Vector[String]): Option[String] = {
    if (xs.isEmpty) None
    else Some(xs.maxBy(_.length))
  }

  def longestLowercase(xs: Vector[String]): Option[String] = {
  if (xs.isEmpty) None
  else Some(xs.filter(_.charAt(0).isLower).maxBy(_.length))
  }
}
