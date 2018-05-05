package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

object Season extends Enumeration {
  type Type = Value

  val Spring = Value(0, "spring")
  val Summer = Value(1, "summer")
  val Fall = Value(2, "fall")
  val Winter = Value(3, "winter")
  val Desolation = Value(4, "desolation")
}

case class xBC_SeasonInfoPacket(season: Season.Type, playSound: Boolean) extends RecvPacket {
  def id: Int = xBC_SeasonInfoPacketParser.packetId
}

object xBC_SeasonInfoPacketParser extends RecvPacketParser {
  val packetId: Int = 0xBC

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val season: Season.Type = Season(data.readByte())
    val playSound: Boolean = data.readByte == 1

    xBC_SeasonInfoPacket(season, playSound)
  }
}
