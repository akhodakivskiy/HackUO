package kdkvsk.hackuo.client

import java.io.DataInput

object IntValue {
  def unapply(value: String): Option[Int] = {
    try {
      Some(value.toInt)
    } catch {
      case _: NumberFormatException =>
        try {
          Some(new java.math.BigInteger(value.stripPrefix("0x"), 16).intValue())
        } catch {
          case _: NumberFormatException => None
        }
    }
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

object DataInputUtils {
  implicit class DataInputOps(dis: DataInput) {
    def readIntLE(): Int = {
      val ch1: Int = dis.readUnsignedByte()
      val ch2: Int = dis.readUnsignedByte()
      val ch3: Int = dis.readUnsignedByte()
      val ch4: Int = dis.readUnsignedByte()
      (ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0)
    }

    def readShortLE(): Int = {
      val ch1: Int = dis.readUnsignedByte()
      val ch2: Int = dis.readUnsignedByte()
      (ch2 << 8) + (ch1 << 0)
    }
  }
}