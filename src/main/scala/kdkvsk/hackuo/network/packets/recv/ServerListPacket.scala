package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream
import java.net.InetAddress

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class ServerInfo(index: Char, name: String, full: Byte, timezone: Byte, ip: InetAddress)

case class ServerListPacket(infoFlag: Int, servers: Seq[ServerInfo]) extends RecvPacket {
  def id: Int = ServerListPacketParser.packetId
}

object ServerListPacketParser extends RecvPacketParser {
  def packetId: Int = 0xA8
  def parse(data: DataInputStream, size: Int): ServerListPacket = {
    ensureLength(data, size)

    val infoFlag: Byte = data.readByte()
    val serversCount: Char = data.readChar()

    val servers: Array[ServerInfo] = Array.fill(serversCount)(null)

    Range(0, serversCount).foreach { i =>
      val index: Char = data.readChar()
      val name: String = readStringWithNull(data, 32)
      val full: Byte = data.readByte()
      val tz: Byte = data.readByte()
      val ip: Int = data.readInt()

      val server = ServerInfo(index, name, full, tz, intToAddressReversed(ip))

      servers(i) = server
    }

    ServerListPacket(infoFlag, servers)
  }
}
