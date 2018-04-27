package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class PlaySoundPacket(flags: Byte, soundModel: Short, volume: Short, x: Short, y: Short, z: Short) extends RecvPacket

object PlaySoundPacketParser extends RecvPacketParser {
  val packetId: Int = 0x54

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val flags: Byte = data.readByte()
    val soundModel: Short = data.readShort()
    val volume: Short = data.readShort()
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    val z: Short = data.readShort()

    PlaySoundPacket(flags, soundModel, volume, x, y, z)
  }
}
