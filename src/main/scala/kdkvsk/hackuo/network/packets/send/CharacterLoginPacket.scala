package kdkvsk.hackuo.network.packets.send

import java.net.InetAddress
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import kdkvsk.hackuo.network.SendPacket

object ClientFlag extends Enumeration {
  type Type = Value

  val T2a = Value(0x00, "t2a")
  val Renaissance = Value(0x01, "renaissance")
  val ThirdDawn = Value(0x02, "third dawn")
  val Lbr = Value(0x04, "lbr")
  val Aos = Value(0x08, "aos")
  val Se = Value(0x10, "se")
  val Sa = Value(0x20, "sa")
  val Uo3d = Value(0x40, "uo3d")
  val Reserved = Value(0x80, "reserved")
  val Client3D = Value(0x100, "3d")

  implicit val clientFlagReader: scopt.Read[Type] = scopt.Read.reads(withName)
}

case class CharacterLoginPacket(characterName: String, characterSlot: Int, clientFlag: ClientFlag.Type, loginCount: Int, address: InetAddress) extends SendPacket {
  val id: Int = 0x5D

  val length: Int = 73

  def serialize(out: ByteBuffer): Unit = {
    out.putInt(0xEDEDEDED) // magic number

    out.put(characterName.getBytes(StandardCharsets.UTF_8))
    out.put(Array.fill(30 - characterName.length)(0x0.byteValue()))

    out.putChar(0) //unknown
    out.putInt(clientFlag.id)
    out.putInt(0x0)
    out.putInt(loginCount)
    out.put(Array.fill(16)(0x0.byteValue()))
    out.putInt(characterSlot)
    out.put(address.getAddress)
  }
}
