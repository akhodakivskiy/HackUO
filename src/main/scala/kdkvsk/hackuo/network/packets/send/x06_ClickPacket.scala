package kdkvsk.hackuo.network.packets.send

import java.nio.ByteBuffer

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.SendPacket

trait ClickPacket extends SendPacket {
  def length: Int = 5
  def serial: Int

  def serialize(out: ByteBuffer): Unit = {
    out.putInt(serial)
  }
}

case class x06_DoubleClickPacket(serial: Int) extends ClickPacket {
  def id: Int = 0x06
}

case class x09_SingleClickPacket(serial: Int) extends ClickPacket {
  def id: Int = 0x09
}
