package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class xA1_HealthUpdatePacket(serial: Serial, maxHealth: Short, health: Short) extends RecvPacket

object xA1_HealthUpdatePacketParser extends RecvPacketParser {
  val packetId: Int = 0xA1

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())
    val maxHealth: Short = data.readShort()
    val health: Short = data.readShort()

    xA1_HealthUpdatePacket(serial, maxHealth, health)
  }
}
