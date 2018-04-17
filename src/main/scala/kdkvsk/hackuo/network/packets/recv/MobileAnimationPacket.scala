package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class MobileAnimationPacket(serial: Serial, actionType: Short, subActionType: Short, subSubActionType: Byte) extends RecvPacket  {
  def id: Int = MobileAnimationPacketParser.packetId
}

object MobileAnimationPacketParser extends RecvPacketParser {
  val packetId: Int = 0xE2

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val actionType: Short = data.readShort()
    val subActionType: Short = data.readShort()
    val subSubActionType: Byte=data.readByte()

    MobileAnimationPacket(serial, actionType, subActionType, subSubActionType)
  }
}
