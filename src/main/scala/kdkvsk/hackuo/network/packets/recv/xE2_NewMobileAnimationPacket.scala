package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class xE2_NewMobileAnimationPacket(serial: Serial, actionType: Short, subActionType: Short, subSubActionType: Byte) extends RecvPacket  {
  def id: Int = xE2_NewMobileAnimationPacketParser.packetId
}

object xE2_NewMobileAnimationPacketParser extends RecvPacketParser {
  val packetId: Int = 0xE2

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val actionType: Short = data.readShort()
    val subActionType: Short = data.readShort()
    val subSubActionType: Byte=data.readByte()

    xE2_NewMobileAnimationPacket(serial, actionType, subActionType, subSubActionType)
  }
}
