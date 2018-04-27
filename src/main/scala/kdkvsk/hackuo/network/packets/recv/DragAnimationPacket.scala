package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class DragAnimationPacket(modelId: Short, amount: Short,
                               sourceSerial: Serial, sourceX: Short, sourceY: Short, sourceZ: Byte,
                               targetSerial: Serial, targetX: Short, targetY: Short, targetZ: Byte) extends RecvPacket

object DragAnimationPacketParser extends RecvPacketParser {
  val packetId: Int = 0x23

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val modelId: Short = data.readShort()
    data.skipBytes(3)
    val amount: Short = data.readShort()
    val sourceSerial: Serial = Serial(data.readInt())
    val sourceX: Short = data.readShort()
    val sourceY: Short = data.readShort()
    val sourceZ: Byte = data.readByte()
    val targetSerial: Serial = Serial(data.readInt())
    val targetX: Short = data.readShort()
    val targetY: Short = data.readShort()
    val targetZ: Byte = data.readByte()

    DragAnimationPacket(modelId, amount, sourceSerial, sourceX, sourceY, sourceZ, targetSerial, targetX, targetY, targetZ)
  }
}
