package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{GraphicId, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class AddContainerItem(serial: Serial, graphicId: GraphicId, amount: Short, x: Short, y: Short, gridIndexOpt: Option[Byte], containerSerial: Serial, hue: Short)

case class AddContainerItemsPacket(items: Seq[AddContainerItem]) extends RecvPacket

object AddContainerItemsPacketParser extends RecvPacketParser {
  val packetId: Int = 0x3C

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    val numItems: Int = data.readShort()
    val hasGridByte: Boolean = (size - 5) / numItems == 20

    val items: Array[AddContainerItem] = new Array(numItems)

    Range(0, numItems).foreach { i =>
      val serial: Serial = Serial(data.readInt())
      val graphicId: GraphicId = GraphicId(data.readShort())
      data.skipBytes(1)
      val amount: Short = data.readShort()
      val x: Short = data.readShort()
      val y: Short = data.readShort()
      val gridIndexOpt: Option[Byte] = if (hasGridByte) Some(data.readByte()) else None
      val containerSerial: Serial = Serial(data.readInt())
      val hue: Short = data.readShort()

      items(i) = AddContainerItem(serial, graphicId, amount, x, y, gridIndexOpt, containerSerial, hue)
    }

    AddContainerItemsPacket(items)
  }
}
