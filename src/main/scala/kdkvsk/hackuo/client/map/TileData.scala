package kdkvsk.hackuo.client.map

import java.io.{BufferedInputStream, DataInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging

object TileFlag extends Enumeration {
  type Type = Value

  val Background = Value(0, "Background")
  val Weapon = Value(1, "Weapon")
  val Transparent = Value(2, "Transparent")
  val Translucent = Value(3, "Translucent")
  val Wall = Value(4, "Wall")
  val Damaging = Value(5, "Damaging")
  val Impassable = Value(6, "Impassable")
  val Wet = Value(7, "Wet")
  val Unknown1 = Value(8, "Unknown1")
  val Surface = Value(9, "Surface")
  val Bridge = Value(10, "Bridge")
  val Generic = Value(11, "Generic")
  val Window = Value(12, "Window")
  val NoShoot = Value(13, "NoShoot")
  val ArticleA = Value(14, "ArticleA")
  val ArticleAn = Value(15, "ArticleAn")
  val Internal = Value(16, "Internal")
  val Foliage = Value(17, "Foliage")
  val PartialHue = Value(18, "PartialHue")
  val Unknown2 = Value(19, "Unknown2")
  val Map = Value(20, "Map")
  val Container = Value(21, "Container")
  val Wearable = Value(22, "Wearable")
  val LightSource = Value(23, "LightSource")
  val Animation = Value(24, "Animation")
  val NoDiagonal = Value(25, "NoDiagonal")
  val Unknown3 = Value(26, "Unknown3")
  val Armor = Value(27, "Armor")
  val Roof = Value(28, "Roof")
  val Door = Value(29, "Door")
  val StairBack = Value(30, "StairBack")
  val StairRight = Value(31, "StairRight")
}

case class LandData(name: String, textureId: Int, flags: TileFlag.ValueSet)
case class ItemData(name: String, flags: TileFlag.ValueSet, weight: Int, quality: Int, quantity: Int, value: Int, height: Int)

case class TileData(landData: Array[LandData], itemData: Array[ItemData]) {
  val maxLandId: Int = landData.length - 1
  val maxItemId: Int = itemData.length - 1

  def land(id: Int): LandData = landData(id & maxLandId)
  def item(id: Int): ItemData = itemData(id & maxItemId)
}

object TileData extends LazyLogging {
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
    logger.info(s"loading tile data from $path")

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

          landData(i) = LandData(name, textureId, TileFlag.ValueSet.fromBitMask(Array(flags)))
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

          itemData(i) = ItemData(name, TileFlag.ValueSet.fromBitMask(Array(flags)), weight, quality, quantity, value, height)
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

          landData(i) = LandData(name, textureId, TileFlag.ValueSet.fromBitMask(Array(flags)))
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

          itemData(i) = ItemData(name, TileFlag.ValueSet.fromBitMask(Array(flags)), weight, quality, quantity, value, height)
        }

        TileData(landData, itemData)
      }
    } finally {
      dis.close()
    }
  }
}
