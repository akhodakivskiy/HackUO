package kdkvsk.hackuo.network.packets.recvsend

import java.io.DataInputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import kdkvsk.hackuo.model.common.ClientVersion
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser, SendPacket}

case class ClientVersionPacket(version: String) extends SendPacket with RecvPacket {
  val id: Int = 0xBD

  val data: Array[Byte] = version.getBytes(StandardCharsets.UTF_8)

  val length: Int = data.length + 3

  def serialize(out: ByteBuffer): Unit = {
    out.putChar(length.toChar)
    out.put(data)
  }
}

object ClientVersionPacketParser extends RecvPacketParser {
  val packetId: Int = 0xBD

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val length: Int = data.readChar()
    if (length > 3) {
      val version = readStringWithNull(data, length - 3)
      ClientVersionPacket(version)
    } else {
      ClientVersionPacket("")
    }
  }
}
