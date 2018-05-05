package kdkvsk.hackuo.network.packets.send

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import kdkvsk.hackuo.network.SendPacket

case class x80_AccountLoginPacket(username: String, password: String, nextLoginKey: Int) extends SendPacket {
  val id: Int = 0x80
  val length: Int = 62

  override def serialize(out: ByteBuffer): Unit = {
    out.put(username.getBytes(StandardCharsets.UTF_8))
    out.put(Array.fill(30 - username.length)(0.byteValue()))
    out.put(password.getBytes(StandardCharsets.UTF_8))
    out.put(Array.fill(30 - password.length)(0.byteValue()))
    out.put(nextLoginKey.toByte)
  }
}
