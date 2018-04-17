package kdkvsk.hackuo.lib

object IntValue {
  def unapply(value: String): Option[Int] = {
    try { Some(value.toInt) } catch { case _: NumberFormatException => None }
  }
}

object DoubleValue {
  def unapply(value: String): Option[Double] = {
    try { Some(value.toDouble) } catch { case _: NumberFormatException => None }
  }
}

object BooleanValue {
  def unapply(value: String): Option[Boolean] = {
    try { Some(value.toBoolean) } catch { case _: NumberFormatException => None }
  }
}
