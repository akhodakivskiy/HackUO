package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

import scala.collection.mutable

case class ClilocItem(clilocId: Int, textOpt: Option[String])

case class ClilocResponsePacket(serial: Serial, items: Seq[ClilocItem]) extends RecvPacket

object ClilocResponsePacketParser extends RecvPacketParser {
  val packetId: Int = 0xD6

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    data.skipBytes(2) // always 0x01
    val serial: Serial = Serial(data.readInt())
    data.skipBytes(2) // always 0x00
    val secondSerial: Int = data.readInt()

    val items: mutable.Buffer[ClilocItem] = mutable.ListBuffer.empty

    var continue: Boolean = true

    while (continue) {
      val clilocId: Int = data.readInt()
      if (clilocId == 0) {
        continue = false
      } else {
        val textLength: Short = data.readShort()
        val textOpt: Option[String] = if (textLength > 0) {
          Some(readUTF16LE(data, textLength))
        } else {
          None
        }

        items.append(ClilocItem(clilocId, textOpt))
      }
    }

    ClilocResponsePacket(serial, items)
  }
}
