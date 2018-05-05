package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x5B_TimePacket(hour: Byte, minute: Byte, second: Byte) extends RecvPacket {
  def id: Int = x5B_TimePacketParser.packetId
}

object x5B_TimePacketParser extends RecvPacketParser {
  val packetId: Int = 0x5B

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val hour: Byte = data.readByte()
    val minute: Byte = data.readByte()
    val second: Byte = data.readByte()
    x5B_TimePacket(hour, minute, second)
  }
}
