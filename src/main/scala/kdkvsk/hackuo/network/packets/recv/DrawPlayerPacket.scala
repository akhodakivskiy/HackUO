package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{BodyId, Direction, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class DrawPlayerPacket(serial: Serial, bodyId: BodyId, hue: Short, flags: Byte, x: Short, y: Short, facing: Byte, z: Byte) extends RecvPacket {
  def id: Int = DrawPlayerPacketParser.packetId

  def direction: Direction.Type = Direction.fromByte(facing)
}

object DrawPlayerPacketParser extends RecvPacketParser {
  def packetId: Int = 0x20

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val bodyId: BodyId = BodyId(data.readShort())
    data.skipBytes(1)
    val skinHue: Short = data.readShort()
    val flags: Byte = data.readByte()
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    data.skipBytes(2)
    val facing: Byte = data.readByte()
    val z: Byte = data.readByte()

    DrawPlayerPacket(serial, bodyId, skinHue, flags, x, y, facing, z)
  }
}
