package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class SetWeatherPacket(weatherType: Byte, intensity: Byte, temperature: Byte) extends RecvPacket {
  def id: Int = SetWeatherPacketParser.packetId
}

object SetWeatherPacketParser extends RecvPacketParser {
  val packetId: Int = 0x65

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val weatherType: Byte = data.readByte()
    val intensity: Byte = data.readByte()
    val temperature: Byte = data.readByte()

    SetWeatherPacket(weatherType, intensity, temperature)
  }
}
