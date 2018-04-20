package kdkvsk.hackuo.network.packets.send

import java.nio.ByteBuffer

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.SendPacket

case class RequestClilocPacket(serials: Seq[Serial]) extends SendPacket {
  def id: Int = 0xD6

  def length: Int = 3 + serials.length * 4

  def serialize(out: ByteBuffer): Unit = {
    out.putShort(length.shortValue())
    serials.foreach { s =>
      out.putInt(s.value)
    }
  }
}
