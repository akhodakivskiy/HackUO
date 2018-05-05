package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x4F_OverallLightLevelPacket(level: Byte) extends RecvPacket {
  def id: Int = x4F_OverallLightLevelPacketParser.packetId
  def length: Int = 2
}

object x4F_OverallLightLevelPacketParser extends RecvPacketParser {
  def packetId: Int = 0x4F

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    x4F_OverallLightLevelPacket(data.readByte())
  }
}

case class x4E_PersonalLightLevelPacket(serial: Int, level: Byte) extends RecvPacket {
  def id: Int = x4E_PersonalLightLevelPacketParser.packetId
}

object x4E_PersonalLightLevelPacketParser extends RecvPacketParser {
  def packetId: Int = 0x4E

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    x4E_PersonalLightLevelPacket(data.readInt(), data.readByte())
  }
}
