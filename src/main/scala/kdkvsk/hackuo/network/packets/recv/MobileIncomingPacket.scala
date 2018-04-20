package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.Notoriety
import kdkvsk.hackuo.model.common.{BodyId, Direction, GraphicId, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

import scala.collection.mutable

case class MobileIncomingItem(serial: Serial, graphicId: GraphicId, layer: Byte, hue: Short)

case class MobileIncomingPacket(serial: Serial, bodyId: BodyId, x: Short, y: Short, z: Short, facing: Byte,
                                hue: Short, flags: Byte, notorietyByte: Byte,
                                items: Seq[MobileIncomingItem]) extends RecvPacket {
  def id: Int = MobileIncomingPacketParser.packetId

  def direction: Direction.Type = Direction.fromByte(facing)

  def notoriety: Notoriety.Type = Notoriety(notorietyByte)
}

object MobileIncomingPacketParser extends RecvPacketParser {
  def packetId: Int = 0x78

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    val serial: Serial = Serial(data.readInt())
    val bodyId: BodyId = BodyId(data.readShort())
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    val z: Byte = data.readByte()
    val facing: Byte = data.readByte()
    val hue: Short = data.readShort()
    val statusFlag: Byte = data.readByte()
    val notoriety: Byte = data.readByte()

    val items: mutable.ListBuffer[MobileIncomingItem] = mutable.ListBuffer.empty

    var continue: Boolean = true

    while (continue) {
      data.mark(4)
      continue = data.readInt() != 0
      data.reset()

      if (continue) {
        val itemSerial: Serial = Serial(data.readInt())
        val itemGraphic: Short = data.readShort()
        val layer: Byte = data.readByte()
        val itemHue: Short = data.readShort()

        items.append(MobileIncomingItem(itemSerial, GraphicId(itemGraphic), layer, itemHue))
      }
    }

    if (data.readInt() != 0) {
      throw new IllegalStateException(s"unexpected end of packet")
    }

    MobileIncomingPacket(serial, bodyId, x, y, z, facing, hue, statusFlag, notoriety, items.toVector)
  }
}
