package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class xAE_MessagePacket(serial: Serial, name: String, graphic: Int, textType: Byte, hue: Short, font: Short, text: String, language: String = "ENU") extends RecvPacket {
  def id: Int = xAE_MessageUnicodePacketParser.packetId
}

object xAE_MessageUnicodePacketParser extends RecvPacketParser with LazyLogging {
  val packetId: Int = 0xAE

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    val serial: Serial = Serial(data.readInt())
    val graphic: Short = data.readShort()
    val textType: Byte = data.readByte()
    val hue: Short = data.readShort()
    val font: Short = data.readShort()
    val language: String = readStringWithNull(data, 4)
    val name: String = readStringWithNull(data, 30)
    val text: String = readUTF16BE(data, size - 48 - 2)
    val nullPlug: Short = data.readShort()

    if (nullPlug != 0) {
      logger.warn(f"expecting to see trailing 0x00, instead seen $nullPlug%02x")
    }

    xAE_MessagePacket(serial, name, graphic, textType, hue, font, text, language)
  }
}

object x1C_MessageAsciiPacketParser extends RecvPacketParser with LazyLogging {
  val packetId: Int = 0x1C

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    val serial: Serial = Serial(data.readInt())
    val graphic: Short = data.readShort()
    val textType: Byte = data.readByte()
    val hue: Short = data.readShort()
    val font: Short = data.readShort()
    val name: String = readStringWithNull(data, 30)
    val text: String = readStringWithNull(data, size - 44)

    xAE_MessagePacket(serial, name, graphic, textType, hue, font, text)
  }
}
