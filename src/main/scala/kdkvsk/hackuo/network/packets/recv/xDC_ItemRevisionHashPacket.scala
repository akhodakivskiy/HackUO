package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class xDC_ItemRevisionHashPacket(serial: Serial, itemHash: Int) extends RecvPacket {
  def id: Int = xDC_ItemRevisionHashPacketParser.packetId
}

object xDC_ItemRevisionHashPacketParser extends RecvPacketParser {
  val packetId: Int = 0xDC

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val hash: Int = data.readInt()

    xDC_ItemRevisionHashPacket(serial, hash)
  }
}
