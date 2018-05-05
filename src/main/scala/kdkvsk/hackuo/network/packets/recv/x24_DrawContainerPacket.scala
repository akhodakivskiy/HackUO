package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{GumpId, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x24_DrawContainerPacket(serial: Serial, gumpId: GumpId) extends RecvPacket

object x24_DrawContainerPacketParser extends RecvPacketParser {
  val packetId: Int = 0x24

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val gumpId: GumpId = GumpId(data.readShort())

    x24_DrawContainerPacket(serial, gumpId)
  }
}
