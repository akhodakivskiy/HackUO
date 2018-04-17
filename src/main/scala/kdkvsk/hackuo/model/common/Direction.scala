package kdkvsk.hackuo.model.common

object Direction extends Enumeration {
  type Type = Value

  val North = Value(0x0, "north")
  val Right = Value(0x1, "right")
  val East = Value(0x2, "east")
  val Down = Value(0x3, "down")
  val South = Value(0x4, "south")
  val Left = Value(0x5, "left")
  val West = Value(0x6, "west")
  val Up = Value(0x7, "up")

  val Mask: Int = 0x7
  val Running: Int = 0x80
  val ValueMask: Int = 0x87

  def fromByte(v: Byte): Type = {
    val i: Int = v & 0xFF
    if ((i & ValueMask) != 0) {
      Up
    } else {
      apply(i)
    }
  }
}
