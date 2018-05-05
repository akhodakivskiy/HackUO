package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{BodyId, Direction, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x77_UpdateMobilePacket(serial: Serial, bodyId: BodyId,
                                  x: Short, y: Short, z: Byte,
                                  direction: Direction.Type,
                                  hue: Short, flags: Byte, highlightColor: Byte) extends RecvPacket {
  def id: Int = x77_UpdateMobilePacketParser.packetId
}

object x77_UpdateMobilePacketParser extends RecvPacketParser {
  val packetId: Int = 0x77

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val bodyId: BodyId = BodyId(data.readShort())
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    val z: Byte = data.readByte()
    val direction: Direction.Type = Direction.fromByte(data.readByte())
    val hue: Short = data.readShort()
    val flag: Byte = data.readByte()
    val highlightColor: Byte = data.readByte()

    x77_UpdateMobilePacket(serial, bodyId, x, y, z, direction, hue, flag, highlightColor)
  }
}

