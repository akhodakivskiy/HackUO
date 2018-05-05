package kdkvsk.hackuo.network.packets.send

import java.net.InetAddress
import java.nio.ByteBuffer

import kdkvsk.hackuo.model.common.ClientVersion
import kdkvsk.hackuo.network.SendPacket

case class xEF_SeedPacket(ip: InetAddress, version: ClientVersion) extends SendPacket {
  val id: Int = 0xEF
  val length = 21

  override def serialize(out: ByteBuffer): Unit = {
    out.putInt(xEF_SeedPacket.seed(ip))
    out.putInt(version.major)
    out.putInt(version.minor)
    out.putInt(version.revision)
    out.putInt(version.patch)
  }
}

object xEF_SeedPacket {
  def seed(ip: InetAddress): Int = {
    var result: Int = 0

    ip.getAddress.foreach { b =>
      result = (result << 8) | (b & 0xFF)
    }

    result
  }
}