package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{Direction, GraphicId, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class MultiInfoPacket(serial: Serial, graphicId: GraphicId, x: Short, y: Short, z: Byte) extends RecvPacket {
  def id: Int = ItemInfoPacketParser.packetId
}

case class ItemInfoPacket(serial: Serial, typeId: GraphicId, amount: Short, x: Short, y: Short, z: Byte, layer: Byte, hue: Short, facing: Byte, flag: Byte) extends RecvPacket {
  def id: Int = ItemInfoPacketParser.packetId

  def direction: Direction.Type = Direction.fromByte(facing)
}

object ItemInfoPacketParser extends RecvPacketParser {
  val packetId: Int = 0xF3

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    data.skipBytes(2)
    val dataType: Byte = data.readByte
    val serial: Serial = Serial(data.readInt())
    val graphicId: GraphicId = GraphicId(data.readShort())
    val facing: Byte = data.readByte()
    val amount: Short = data.readShort()
    val _: Short = data.readShort()
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    val z: Byte = data.readByte()
    val layer: Byte = data.readByte()
    val hue: Short = data.readShort()
    val flag: Byte = data.readByte()

    if (dataType == 0x0) {
      ItemInfoPacket(serial, graphicId, amount, x, y, z, layer, hue, facing, flag)
    } else {
      MultiInfoPacket(serial, graphicId, x, y, z)
    }

  }
}
