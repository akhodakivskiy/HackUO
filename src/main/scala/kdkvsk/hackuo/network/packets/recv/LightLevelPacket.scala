package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class OverallLightLevelPacket(level: Byte) extends RecvPacket {
  def id: Int = OverallLightLevelPacketParser.packetId
  def length: Int = 2
}

object OverallLightLevelPacketParser extends RecvPacketParser {
  def packetId: Int = 0x4F

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    OverallLightLevelPacket(data.readByte())
  }
}

case class PersonalLightLevelPacket(serial: Int, level: Byte) extends RecvPacket {
  def id: Int = PersonalLightLevelPacketParser.packetId
}

object PersonalLightLevelPacketParser extends RecvPacketParser {
  def packetId: Int = 0x4E

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    PersonalLightLevelPacket(data.readInt(), data.readByte())
  }
}
