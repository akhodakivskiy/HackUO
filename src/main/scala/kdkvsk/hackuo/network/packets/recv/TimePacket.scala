package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class TimePacket(hour: Byte, minute: Byte, second: Byte) extends RecvPacket {
  def id: Int = TimePacketParser.packetId
}

object TimePacketParser extends RecvPacketParser {
  val packetId: Int = 0x5B

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val hour: Byte = data.readByte()
    val minute: Byte = data.readByte()
    val second: Byte = data.readByte()
    TimePacket(hour, minute, second)
  }
}
