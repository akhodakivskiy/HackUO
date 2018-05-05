package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x76_ServerChangePacket(x: Int, y: Int, z: Int, boundX: Int, boundY: Int, boundWidth: Int, boundHeight: Int) extends RecvPacket

object x76_ServerChangePacketParser extends RecvPacketParser {
  val packetId: Int = 0x76

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val x: Int = data.readShort() & 0xFFFF
    val y: Int = data.readShort() & 0xFFFF
    val z: Int = data.readShort() & 0xFFFF

    val boundX: Int = data.readShort() & 0xFFFF
    val boundY: Int = data.readShort() & 0xFFFF
    val boundWidth: Int = data.readShort() & 0xFFFF
    val boundHeight: Int = data.readShort() & 0xFFFF

    x76_ServerChangePacket(x, y, z, boundX, boundY, boundWidth, boundHeight)
  }
}
