package kdkvsk.hackuo.network.packets.send

import java.net.InetAddress
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import kdkvsk.hackuo.network.SendPacket

case class x5D_CharacterLoginPacket(characterName: String, characterSlot: Int, clientFlag: Int, loginCount: Int, address: InetAddress) extends SendPacket {
  val id: Int = 0x5D

  val length: Int = 73

  def serialize(out: ByteBuffer): Unit = {
    out.putInt(0xEDEDEDED) // magic number

    out.put(characterName.getBytes(StandardCharsets.UTF_8))
    out.put(Array.fill(30 - characterName.length)(0x0.byteValue()))

    out.putChar(0) //unknown
    out.putInt(clientFlag)
    out.putInt(0x0)
    out.putInt(loginCount)
    out.put(Array.fill(16)(0x0.byteValue()))
    out.putInt(characterSlot)
    out.put(address.getAddress)
  }
}
