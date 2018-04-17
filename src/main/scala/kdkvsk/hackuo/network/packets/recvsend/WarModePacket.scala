package kdkvsk.hackuo.network.packets.recvsend

import java.io.DataInputStream
import java.nio.ByteBuffer

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser, SendPacket}

case class WarModePacket(mode: Byte) extends RecvPacket with SendPacket {
  val id: Int = WarModePacketParser.packetId

  val length: Int = 5

  def serialize(out: ByteBuffer): Unit = {
    out.put(mode)
    out.put(0x0.byteValue())
    out.put(0x32.byteValue())
    out.put(0x00.byteValue())
  }
}

object WarModePacketParser extends RecvPacketParser {
  val packetId: Int = 0x72

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    WarModePacket(data.readByte())
  }
}
