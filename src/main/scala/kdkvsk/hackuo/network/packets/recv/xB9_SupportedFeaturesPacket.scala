package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class xB9_SupportedFeaturesPacket(featuresFlag: Int) extends RecvPacket {
  val id: Int = xB9_SupportedFeaturesPacketParser.packetId
}

object xB9_SupportedFeaturesPacketParser extends RecvPacketParser {
  def packetId: Int = 0xB9
  val buffer: Array[Byte] = Array.fill(32)(0x0)

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val features: Int = if (size <= 3) data.readChar() else data.readInt()
    xB9_SupportedFeaturesPacket(features)
  }
}
