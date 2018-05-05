package kdkvsk.hackuo.network.packets.send

import java.nio.ByteBuffer

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.SendPacket

object StatusType extends Enumeration {
  type Type = Value

  val GodClient = Value(0, "god")
  val Basic = Value(4, "basic")
  val Skills = Value(5, "skills")
}

case class x34_RequestStatusPacket(statusType: StatusType.Type, serial: Serial) extends SendPacket {
  def id: Int = 0x34

  def length: Int = 10

  def serialize(out: ByteBuffer): Unit = {
    out.putInt(0xEDEDEDED)
    out.put(statusType.id.byteValue())
    out.putInt(serial.value)
  }
}
