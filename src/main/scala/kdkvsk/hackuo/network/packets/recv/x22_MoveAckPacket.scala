package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream
import java.nio.ByteBuffer

import kdkvsk.hackuo.model.Notoriety
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser, SendPacket}

case class x22_MoveAckPacket(sequence: Byte, notoriety: Notoriety.Type) extends RecvPacket with SendPacket {
  def id: Int = x22_MoveAckPacketParser.packetId

  def length: Int = 3

  def serialize(out: ByteBuffer): Unit = {
    out.put(sequence)
    out.put(notoriety.id.byteValue())
  }
}

object x22_MoveAckPacketParser extends RecvPacketParser {
  val packetId: Int = 0x22

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val sequence: Byte = data.readByte()
    val notoriety: Notoriety.Type = Notoriety(data.readByte())

    x22_MoveAckPacket(sequence, notoriety)
  }
}

