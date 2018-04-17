package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class ItemRevisionHashPacket(serial: Serial, itemHash: Int) extends RecvPacket {
  def id: Int = ItemRevisionHashPacketParser.packetId
}

object ItemRevisionHashPacketParser extends RecvPacketParser {
  val packetId: Int = 0xDC

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val hash: Int = data.readInt()

    ItemRevisionHashPacket(serial, hash)
  }
}
