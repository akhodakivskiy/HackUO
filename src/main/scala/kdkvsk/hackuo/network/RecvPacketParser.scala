package kdkvsk.hackuo.network

import java.io.DataInputStream
import java.math.BigInteger
import java.net.InetAddress
import java.nio.charset.StandardCharsets

trait RecvPacketParser {
  def packetId: Int

  def parse(data: DataInputStream, size: Int): RecvPacket

  def intToAddress(value: Int): InetAddress = {
    InetAddress.getByAddress(BigInteger.valueOf(value).toByteArray)
  }

  def intToAddressReversed(value: Int): InetAddress = {
    InetAddress.getByAddress(BigInteger.valueOf(value).toByteArray.reverse)
  }

  def readStringWithNull(data: DataInputStream, maxLength: Int): String = {
    val buffer: Array[Byte] = Array.fill(maxLength)(0x0)
    data.read(buffer, 0, maxLength)
    val nameSize: Int = buffer.indexWhere(a => (a & 0xFF) == 0)
    new String(buffer, 0, nameSize, "UTF-8")
  }

  def readString(data: DataInputStream, length: Int): String = {
    val buffer: Array[Byte] = Array.fill(length)(0x0)
    data.read(buffer, 0, length)
    new String(buffer, 0, length, "UTF-8")
  }

  def ensureLength(data: DataInputStream, length: Int): Unit = {
    val packetLength: Int = data.readShort()

    if (packetLength != length) {
      throw new IllegalStateException(f"packet $packetId%02x expected length is $length, server sent $packetLength")
    }
  }

  def readUnicode(data: DataInputStream, maxLength: Int): String = {
    val buffer: Array[Byte] = new Array(maxLength)
    data.read(buffer, 0, maxLength)
    new String(buffer, 0, maxLength, StandardCharsets.UTF_16BE)
  }
}
