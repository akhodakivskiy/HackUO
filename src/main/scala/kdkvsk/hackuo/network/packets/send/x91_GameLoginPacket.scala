package kdkvsk.hackuo.network.packets.send

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import kdkvsk.hackuo.network.SendPacket

case class x91_GameLoginPacket(authId: Int, username: String, password: String) extends SendPacket {
  val id: Int = 0x91
  val length: Int = 65

  def serialize(out: ByteBuffer): Unit = {
    out.putInt(authId)
    out.put(username.getBytes(StandardCharsets.UTF_8))
    out.put(Array.fill(30 - username.length)(0.byteValue()))
    out.put(password.getBytes(StandardCharsets.UTF_8))
    out.put(Array.fill(30 - password.length)(0.byteValue()))
  }
}
