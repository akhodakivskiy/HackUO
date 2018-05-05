package kdkvsk.hackuo.network.packets.recvsend

import java.io.DataInputStream
import java.nio.ByteBuffer

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser, SendPacket}

case class x72_WarModePacket(mode: Byte) extends RecvPacket with SendPacket {
  val id: Int = x72_WarModePacketParser.packetId

  val length: Int = 5

  def serialize(out: ByteBuffer): Unit = {
    out.put(mode)
    out.put(0x0.byteValue())
    out.put(0x32.byteValue())
    out.put(0x00.byteValue())
  }
}

object x72_WarModePacketParser extends RecvPacketParser {
  val packetId: Int = 0x72

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    x72_WarModePacket(data.readByte())
  }
}
