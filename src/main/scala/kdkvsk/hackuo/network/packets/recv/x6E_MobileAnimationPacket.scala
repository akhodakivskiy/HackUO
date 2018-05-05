package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x6E_MobileAnimationPacket(serial: Serial, action: Short, frameCount: Byte, repeatCount: Short, reverse: Byte, repeatFlag: Byte, frameDelay: Byte) extends RecvPacket

object x6E_MobileAnimationPacketParser extends RecvPacketParser {
  val packetId: Int = 0x6E

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val action: Short = data.readShort()
    val frameCount: Byte = data.readByte()
    val repeatCount: Byte = data.readByte()
    val reverse: Byte = data.readByte()
    val repeatFlag: Byte = data.readByte()
    val frameDelay: Byte = data.readByte()

    x6E_MobileAnimationPacket(serial, action, frameCount, repeatCount, reverse, repeatFlag, frameDelay)
  }
}
