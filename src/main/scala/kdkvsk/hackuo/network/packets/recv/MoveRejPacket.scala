package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Direction
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class MoveRejPacket(sequence: Byte, x: Short, y: Short, z: Byte, direction: Direction.Type) extends RecvPacket

object MoveRejPacketParser extends RecvPacketParser {
  val packetId: Int = 0x21

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val sequence: Byte = data.readByte()
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    val direction: Direction.Type = Direction.fromByte(data.readByte())
    val z: Byte = data.readByte()

    MoveRejPacket(sequence, x, y, z, direction)
  }
}
