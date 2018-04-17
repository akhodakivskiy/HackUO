package kdkvsk.hackuo.network.packets.send

import java.nio.ByteBuffer

import kdkvsk.hackuo.network.SendPacket

case class SelectServerPacket(index: Int) extends SendPacket {
  val id: Int = 0xA0
  val length: Int = 3

  def serialize(out: ByteBuffer): Unit = {
    out.putChar(index.toChar)
  }
}
