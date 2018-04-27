package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{GraphicId, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class AddItemToContainerPacket(serial: Serial, graphicId: GraphicId, graphicIdOffset: Byte,
                                    amount: Short, x: Int, y: Int, containerSerial: Serial, hue: Short,
                                    gridIndexOpt: Option[Byte] = None) extends RecvPacket

object AddItemToContainerPacketParser extends RecvPacketParser {
  val packetId: Int = 0x25

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val graphicId: GraphicId = GraphicId(data.readShort())
    val graphicIdOffset: Byte = data.readByte()
    val amount: Short = data.readShort()
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    val slotIndexOpt: Option[Byte] = if (size == 21) Some(data.readByte()) else None
    val containerSerial: Serial = Serial(data.readInt())
    val hue: Short = data.readShort()

    AddItemToContainerPacket(serial, graphicId, graphicIdOffset, amount, x, y, containerSerial, hue, slotIndexOpt)
  }
}
