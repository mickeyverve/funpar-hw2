object OptionFriends extends App {
  def lookup(xs: List[(String, String)], key: String): Option[String] = {
    if (xs.exists(_._1.contains(key))) {
      Some(xs.filter(_._1.equals(key)).head._2)
    }
    else None
  }

//  println(lookup(List(("a", "xy"), ("c", "pq"), ("a", "je")),"a"))

  def resolve(userIdFromLoginName: String => Option[String],
              majorFromUserId: String => Option[String],
              divisionFromMajor: String => Option[String],
              averageScoreFromDivision: String => Option[Double],
              loginName: String): Double = {

    ???
  }
}
