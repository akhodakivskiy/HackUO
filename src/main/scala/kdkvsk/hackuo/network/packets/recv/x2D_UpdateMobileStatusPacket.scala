package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x2D_UpdateMobileStatusPacket(serial: Serial,
                                        maxHits: Short, hits: Short,
                                        maxMana: Short, mana: Short,
                                        maxStamina: Short, stamina: Short) extends RecvPacket

object x2D_UpdateMobileStatusPacketParser extends RecvPacketParser {
  val packetId: Int = 0x2D

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val serial: Serial = Serial(data.readInt())

    val maxHits: Short = data.readShort()
    val hits: Short = data.readShort()
    val maxMana: Short = data.readShort()
    val mana: Short = data.readShort()
    val maxStamina: Short = data.readShort()
    val stamina: Short = data.readShort()

    x2D_UpdateMobileStatusPacket(serial, maxHits, hits, maxMana, mana, maxStamina, stamina)
  }
}
