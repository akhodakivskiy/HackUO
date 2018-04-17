package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{Direction, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class LoginConfirmPacket(serial: Serial, bodyId: Short, x: Short, y: Short, z: Byte, facing: Byte, width: Short, height: Short) extends RecvPacket {
  val id: Int = LoginConfirmPacketParser.packetId

  def direction: Direction.Type = Direction.fromByte(facing)
}

object LoginConfirmPacketParser extends RecvPacketParser {
  val packetId: Int = 0x1B

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    data.skipBytes(4)
    val bodyId: Short = data.readShort()
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    data.skipBytes(1)
    val z: Byte = data.readByte()
    val facing: Byte = data.readByte()
    data.skipBytes(9)
    val width: Short = (data.readShort() + 8).shortValue()
    val height: Short = data.readShort()
    data.skipBytes(6)

    LoginConfirmPacket(serial, bodyId, x, y, z, facing, width, height)
  }
}
