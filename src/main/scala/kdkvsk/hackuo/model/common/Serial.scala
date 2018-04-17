package kdkvsk.hackuo.model.common

case class Serial(value: Int) {
  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  def isMobile: Boolean = value > 0 && value < 0x40000000
  def isItem: Boolean = value >= 0x40000000 && value <= 0x7FFFFFFF
  def isValid: Boolean = value > 0

  def == (serial: Int): Boolean = value == serial
}
