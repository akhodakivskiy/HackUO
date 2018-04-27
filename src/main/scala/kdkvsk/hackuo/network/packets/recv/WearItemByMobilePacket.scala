package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{GraphicId, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class WearItemByMobilePacket(serial: Serial, graphicId: GraphicId, layer: Byte, mobileSerial: Serial, hue: Short) extends RecvPacket

object WearItemByMobilePacketParser extends RecvPacketParser {
  val packetId: Int = 0x2E

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val graphicId: GraphicId = GraphicId(data.readShort())
    data.skipBytes(1)
    val layer: Byte = data.readByte()
    val mobileSerial: Serial = Serial(data.readInt())
    val hue: Short = data.readShort()

    WearItemByMobilePacket(serial, graphicId, layer, mobileSerial, hue)
  }
}


