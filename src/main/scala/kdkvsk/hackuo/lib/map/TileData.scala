package kdkvsk.hackuo.lib.map

import java.io.{BufferedInputStream, DataInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

case class LandData(name: String, textureId: Int, flags: Long)
case class ItemData(name: String, flags: Long, weight: Int, quality: Int, quantity: Int, value: Int, height: Int)

case class TileData(landData: Array[LandData], itemData: Array[ItemData])

object TileData {
  val nameBuffer: Array[Byte] = new Array[Byte](20)

  def readName(dis: DataInputStream): String = {
    if (dis.read(nameBuffer, 0, nameBuffer.length) == nameBuffer.length) {
      val nullIndex: Int = nameBuffer.indexOf(0)
      if (nullIndex >= 0) {
        new String(nameBuffer, 0, nullIndex, StandardCharsets.US_ASCII)
      } else {
        new String(nameBuffer, 0, nameBuffer.length, StandardCharsets.US_ASCII)
      }
    } else {
      throw new IllegalArgumentException(s"failed to read ${nameBuffer.length} bytes")
    }
  }

  def load(path: Path): TileData = {
    if (!Files.exists(path)) {
      throw new IllegalArgumentException(s"tiledata file $path doesn't exist")
    }

    val fileSize: Long = Files.size(path)
    val dis: DataInputStream = new DataInputStream(new BufferedInputStream(Files.newInputStream(path)))

    try {

      if (fileSize >= 3188736) { // 7.0.9.0
        val landData = new Array[LandData](0x4000)

        landData.indices.foreach { i =>
          if (i == 1 || (i > 0 && (i & 0x1F) == 0)) {
            dis.skipBytes(4)
          }

          val flags: Long = dis.readLong()
          val textureId: Int = dis.readShort() & 0xFFFF
          val name: String = readName(dis)

          landData(i) = LandData(name, textureId, flags)
        }

        val itemData: Array[ItemData] = new Array[ItemData](0x10000)

        itemData.indices.foreach { i =>
          if ((i & 0x1F) == 0) {
            dis.skipBytes(4)
          }

          val flags: Long = dis.readLong()
          val weight: Int = dis.readByte() & 0xFF
          val quality: Int = dis.readByte() & 0xFF
          dis.skipBytes(3)
          val quantity: Int = dis.readByte() & 0xFF
          dis.skipBytes(5)
          val value: Int = dis.readByte() & 0xFF
          val height: Int = dis.readByte() & 0xFF
          val name: String = readName(dis)

          itemData(i) = ItemData(name, flags, weight, quality, quantity, value, height)
        }

        TileData(landData, itemData)
      } else {
        val landData = new Array[LandData](0x4000)

        landData.indices.foreach { i =>
          if ((i & 0x1F) == 0) {
            dis.skipBytes(4)
          }

          val flags: Long = dis.readLong()
          val textureId: Short = dis.readShort()
          val name: String = readName(dis)

          landData(i) = LandData(name, textureId, flags)
        }

        val itemDataSize: Int = if (fileSize == 1644544) { // 7.0.0.0
          0x8000
        } else {
          0x4000
        }

        val itemData: Array[ItemData] = new Array[ItemData](itemDataSize)

        itemData.indices.foreach { i =>
          if ((i & 0x1F) == 0) {
            dis.skipBytes(4)
          }

          val flags: Long = dis.readInt()
          val weight: Int = dis.readByte() & 0xFF
          val quality: Int = dis.readByte() & 0xFF
          dis.skipBytes(3)
          val quantity: Int = dis.readByte() & 0xFF
          dis.skipBytes(5)
          val value: Int = dis.readByte() & 0xFF
          val height: Int = dis.readByte() & 0xFF
          val name: String = readName(dis)

          itemData(i) = ItemData(name, flags, weight, quality, quantity, value, height)
        }

        TileData(landData, itemData)
      }
    } finally {
      dis.close()
    }
  }
}
