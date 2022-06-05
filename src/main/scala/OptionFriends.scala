object OptionFriends extends App {
  def lookup(xs: List[(String, String)], key: String): Option[String] = {
    if (xs.exists(_._1.contains(key))) {
      Some(xs.filter(_._1.equals(key)).head._2)
    }
    else None
  }

  def resolve(userIdFromLoginName: String => Option[String],
              majorFromUserId: String => Option[String],
              divisionFromMajor: String => Option[String],
              averageScoreFromDivision: String => Option[Double],
              loginName: String): Double = {
    userIdFromLoginName(loginName).flatMap(
      userId => majorFromUserId(userId).flatMap(
        major => divisionFromMajor(major).flatMap(
          division => averageScoreFromDivision(division)
          )
        )
      ).getOrElse(0.0)
  }
}
