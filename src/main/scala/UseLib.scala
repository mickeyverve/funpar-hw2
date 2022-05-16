object UseLib extends App {
  def onlyBeginsWithLower(xs: Vector[String]): Vector[String] = {
    xs.filter(_.charAt(0).isLower)
  }
//  println(onlyBeginsWithLower(Vector("ADd","DEc","Edd","DDD")))

  def longestString(xs: Vector[String]): Option[String] = {
    if (xs.isEmpty) None
    else Some(xs.maxBy(_.length))
  }
//  println(longestString(Vector("ADd","DEc","Edd","DDD","dasasfdasff","fsdfdshushfskjhkfhds")))

  def longestLowercase(xs: Vector[String]): Option[String] = {
  if (xs.isEmpty) None
  else Some(xs.filter(_.charAt(0).isLower).maxBy(_.length))
  }
//  println(longestLowercase(Vector("ADd","DEc","Edd","DDD","dasasfdasff","Fsdfdshushfskjhkfhds")))
}
