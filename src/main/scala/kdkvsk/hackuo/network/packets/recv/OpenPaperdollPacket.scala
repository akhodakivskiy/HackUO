package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class OpenPaperdollPacket(serial: Serial, title: String, flag: Byte) extends RecvPacket {

}

object OpenPaperdollPacketParser extends RecvPacketParser {
  val packetId: Int = 0x88

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val payerId: Serial = Serial(data.readInt())
    val title: String = readStringWithNull(data, 60)
    val flag: Byte = data.readByte()

    OpenPaperdollPacket(payerId, title, flag)
  }
}
