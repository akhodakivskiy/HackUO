package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x17_HealthColorPacket(serial: Int, color: Short, flag: Byte) extends RecvPacket {
  def id: Int = x17_HealthColorPacketParser.packetId
}

object x17_HealthColorPacketParser extends RecvPacketParser {
  val packetId: Int = 0x17

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, 12)

    val serial: Int = data.readInt()
    data.skipBytes(2)
    val color: Short = data.readShort()
    val flag: Byte = data.readByte()

    x17_HealthColorPacket(serial, color, flag)
  }
}
