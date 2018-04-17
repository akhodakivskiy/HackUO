package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class HealthColorPacket(serial: Int, color: Short, flag: Byte) extends RecvPacket {
  def id: Int = HealthColorPacketParser.packetId
}

object HealthColorPacketParser extends RecvPacketParser {
  val packetId: Int = 0x17

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, 12)

    val serial: Int = data.readInt()
    data.skipBytes(2)
    val color: Short = data.readShort()
    val flag: Byte = data.readByte()

    HealthColorPacket(serial, color, flag)
  }
}
