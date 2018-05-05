package kdkvsk.hackuo.network.packets.recvsend

import java.io.DataInputStream
import java.nio.ByteBuffer

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser, SendPacket}

case class xC8_ViewRangePacket(range: Int) extends RecvPacket with SendPacket {
  val id: Int = xC8_ViewRangePacketParser.packetId

  def length: Int = 2

  def serialize(out: ByteBuffer): Unit = {
    out.put(range.byteValue())
  }
}

object xC8_ViewRangePacketParser extends RecvPacketParser {
  val packetId: Int = 0xC8

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    xC8_ViewRangePacket(data.readByte())
  }
}
