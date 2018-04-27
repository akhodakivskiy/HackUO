package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.{GraphicId, Serial}
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class GraphicalEffectPacket(effectType: Byte, sourceSerial: Serial, targetSerial: Serial, graphicId: GraphicId, x: Short, y: Short, z: Byte,
                                 speed: Byte, duration: Byte, explodes: Byte, hue: Int, renderMode: Int) extends RecvPacket

object GraphicalEffectPacketParser extends RecvPacketParser {
  val packetId: Int = 0xC0

  def parse(data: DataInputStream, size: Int): RecvPacket = {

    val effectType: Byte = data.readByte()
    val sourceSerial: Serial = Serial(data.readInt())
    val targetSerial: Serial = Serial(data.readInt())
    val graphicId: GraphicId = GraphicId(data.readShort())
    val x: Short = data.readShort()
    val y: Short = data.readShort()
    val z: Byte = data.readByte()
    val speed: Byte = data.readByte()
    data.skipBytes(2)
    val duration: Byte = data.readByte()
    val explodes: Byte = data.readByte()
    val hue: Int = data.readInt()
    val renderMode: Int = data.readInt()

    GraphicalEffectPacket(effectType, sourceSerial, targetSerial, graphicId, x, y, z, speed, duration, explodes, hue, renderMode)
  }
}
