package kdkvsk.hackuo.network.packets.send

import java.nio.ByteBuffer

import kdkvsk.hackuo.model.common.Direction
import kdkvsk.hackuo.network.SendPacket

case class MoveRequestPacket(direction: Direction.Type, isRunning: Boolean, sequence: Int, fastWalkKey: Int) extends SendPacket {
  def id: Int = 0x02

  def length: Int = 7

  def serialize(out: ByteBuffer): Unit = {
    val facing: Int = if (isRunning) direction.id | Direction.Running else direction.id
    out.put(facing.byteValue())
    out.put(sequence.byteValue())
    out.putInt(fastWalkKey)
  }
}
