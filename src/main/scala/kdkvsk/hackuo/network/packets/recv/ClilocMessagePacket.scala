package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.lib.Cliloc
import kdkvsk.hackuo.model.common.{BodyId, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class ClilocMessagePacket(serial: Serial, bodyId: BodyId, location: Byte, hue: Short, font: Short, clilocId: Int, speakerName: String, args: String, textOpt: Option[String]) extends RecvPacket {
  def isSystem: Boolean = serial.value == 0xFFFFFFFF || bodyId.value == 0xFFFF
}

case class ClilocMessagePacketParser(cliloc: Cliloc) extends RecvPacketParser {
  val packetId: Int = 0xC1

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    val serial: Serial = Serial(data.readInt())
    val bodyId: BodyId = BodyId(data.readShort())
    val location: Byte = data.readByte()
    val hue: Short = data.readShort()
    val font: Short = data.readShort()
    val clilocId: Int = data.readInt()
    val speakerName: String = readStringWithNull(data, 30)
    val args: String = readUTF16LE(data, size - 48 - 2) // null terminated (0x0000)

    val textOpt: Option[String] = cliloc.getAndReplace(clilocId, args)

    ClilocMessagePacket(serial, bodyId, location, hue, font, clilocId, speakerName, args, textOpt)
  }
}
