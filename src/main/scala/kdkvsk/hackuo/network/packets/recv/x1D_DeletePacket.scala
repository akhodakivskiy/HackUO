package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x1D_DeletePacket(serial: Serial) extends RecvPacket {
  def id: Int = x1D_DeletePacketParser.packetId
}

object x1D_DeletePacketParser extends RecvPacketParser {
  val packetId: Int = 0x1D

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial = Serial(data.readInt())

    x1D_DeletePacket(serial)
  }
}
