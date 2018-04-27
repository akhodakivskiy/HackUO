package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class HealthUpdatePacket(serial: Serial, maxHealth: Short, health: Short) extends RecvPacket

object HealthUpdatePacketParser extends RecvPacketParser {
  val packetId: Int = 0xA1

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val maxHealth: Short = data.readShort()
    val health: Short = data.readShort()

    HealthUpdatePacket(serial, maxHealth, health)
  }
}
