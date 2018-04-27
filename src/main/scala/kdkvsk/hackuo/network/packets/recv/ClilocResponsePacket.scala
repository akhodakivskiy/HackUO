package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.lib.Cliloc
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

import scala.collection.mutable

case class ClilocItem(clilocId: Int, textOpt: Option[String], args: String)

case class ClilocResponsePacket(serial: Serial, items: Seq[ClilocItem]) extends RecvPacket

case class ClilocResponsePacketParser(cliloc: Cliloc) extends RecvPacketParser {
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
        val argsLength: Short = data.readShort()
        val args: String = if (argsLength > 0) {
          readUTF16LE(data, argsLength)
        } else {
          ""
        }

        val textOpt: Option[String] = cliloc.getAndReplace(clilocId, args)

        items.append(ClilocItem(clilocId, textOpt, args))
      }
    }

    ClilocResponsePacket(serial, items)
  }
}
