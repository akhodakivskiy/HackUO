package kdkvsk.hackuo.network.packets.recvsend

import java.io.DataInputStream
import java.nio.ByteBuffer

import kdkvsk.hackuo.client.Cliloc
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser, SendPacket}

import scala.collection.mutable

case class xD6_RequestClilocPacket(serials: Seq[Serial]) extends SendPacket {
  def id: Int = 0xD6

  def length: Int = 3 + serials.length * 4

  def serialize(out: ByteBuffer): Unit = {
    out.putShort(length.shortValue())
    serials.foreach { s =>
      out.putInt(s.value)
    }
  }
}

case class ClilocItem(clilocId: Int, textOpt: Option[String], args: String)

case class xD6_ClilocResponsePacket(serial: Serial, items: Seq[ClilocItem]) extends RecvPacket

case class xD6_ClilocResponsePacketParser(cliloc: Cliloc) extends RecvPacketParser {
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

    xD6_ClilocResponsePacket(serial, items)
  }
}
