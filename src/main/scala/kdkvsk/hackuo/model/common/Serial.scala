package kdkvsk.hackuo.model.common

case class Serial(value: Int) {
  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  def isMobile: Boolean = value > 0 && value < 0x40000000
  def isItem: Boolean = value >= 0x40000000 && value <= 0x7FFFFFFF
  def isValid: Boolean = value > 0

  def == (serial: Int): Boolean = value == serial

  override val toString: String = f"Serial($value%02x)"
}

trait TypeId2 {
  def value: Short
}

case class BodyId(value: Short) extends TypeId2 {
  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  override val toString: String = f"BodyId($value%02x)"
}

case class GraphicId(value: Short) extends TypeId2 {
  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  override val toString: String = f"GraphicId($value%02x)"
}

case class GumpId(value: Short) extends TypeId2 {
  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  override val toString: String = f"GumpId($value%02x)"
}
