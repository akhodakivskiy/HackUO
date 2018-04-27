package kdkvsk.hackuo.network

import java.io.DataInputStream
import java.nio.ByteBuffer

trait RecvPacket

trait SendPacket {
  def id: Int
  def length: Int
  def serialize(out: ByteBuffer): Unit
}
