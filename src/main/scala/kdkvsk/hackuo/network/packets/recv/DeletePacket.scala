package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class DeletePacket(serial: Serial) extends RecvPacket {
  def id: Int = DeletePacketParser.packetId
}

object DeletePacketParser extends RecvPacketParser {
  val packetId: Int = 0x1D

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial = Serial(data.readInt())

    DeletePacket(serial)
  }
}
