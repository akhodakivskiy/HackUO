package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{BodyId, Direction, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x1B_LoginConfirmPacket(serial: Serial, typeId: BodyId, x: Short, y: Short, z: Byte, facing: Byte, width: Short, height: Short) extends RecvPacket {
  val id: Int = x1B_LoginConfirmPacketParser.packetId

  def direction: Direction.Type = Direction.fromByte(facing)
}

object x1B_LoginConfirmPacketParser extends RecvPacketParser {
  val packetId: Int = 0x1B

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    data.skipBytes(4)
    val bodyId: BodyId = BodyId(data.readShort())
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    data.skipBytes(1)
    val z: Byte = data.readByte()
    val facing: Byte = data.readByte()
    data.skipBytes(9)
    val width: Short = (data.readShort() + 8).shortValue()
    val height: Short = data.readShort()
    data.skipBytes(6)

    x1B_LoginConfirmPacket(serial, bodyId, x, y, z, facing, width, height)
  }
}
