package kdkvsk.hackuo.network.packets.send

import java.nio.ByteBuffer

import kdkvsk.hackuo.network.SendPacket

object xB5_OpenNoneChatWindow extends SendPacket {
  def id: Int = 0xB5

  def length: Int = 64

  def serialize(out: ByteBuffer): Unit = {
    Range(0, 63).foreach { _ =>
      out.put(0x00.byteValue())
    }
  }
}
