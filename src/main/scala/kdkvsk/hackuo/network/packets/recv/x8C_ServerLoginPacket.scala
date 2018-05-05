package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream
import java.net.InetAddress

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x8C_ServerLoginPacket(serverIp: InetAddress, port: Short, authId: Int) extends RecvPacket {
  val id: Int = x8C_ServerLoginPacketParser.packetId
}

object x8C_ServerLoginPacketParser extends RecvPacketParser {
  val packetId: Int = 0x8C

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val address = intToAddress(data.readInt())
    val port: Short = data.readShort()
    val authId: Int = data.readInt()
    x8C_ServerLoginPacket(address, port, authId)
  }
}
