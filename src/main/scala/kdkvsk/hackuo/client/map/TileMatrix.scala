package kdkvsk.hackuo.client.map

import java.io.RandomAccessFile
import java.nio.file.Path

import com.typesafe.scalalogging.LazyLogging
import sun.jvm.hotspot.oops.InstanceKlass.StaticField

import scala.collection.mutable

case class LandTile(id: Int, z: Int)
case class StaticTile(id: Int, z: Int, hue: Int)

class TileMatrix(val fileIndex: Int, width: Int, height: Int, mapPath: Path, withMapIndex: Boolean, staticIndexPath: Path, staticPath: Path) extends LazyLogging {
  import kdkvsk.hackuo.client.DataInputUtils.DataInputOps

  val blockWidth: Int = width >>> 3
  val blockHeight: Int = height >>> 3
  val landTiles: Array[Array[Array[LandTile]]] = new Array(blockWidth)
  val staticTiles: Array[Array[Array[Array[mutable.Buffer[StaticTile]]]]] = new Array(blockWidth)

  val uopIndexOpt: Option[UopIndex] = if (withMapIndex) Some(UopIndex.load(mapPath)) else None
  val mapFile: RandomAccessFile = new RandomAccessFile(mapPath.toString, "r")
  val staticIndexFile: RandomAccessFile = new RandomAccessFile(staticIndexPath.toString, "r")
  val staticFile: RandomAccessFile = new RandomAccessFile(staticPath.toString, "r")

  logger.info(s"starting populating tile matrix #$fileIndex, size $width x $height from $mapPath, map index: $withMapIndex")

  def getLandTile(x: Int, y: Int): LandTile = {
    val tiles: Array[LandTile] = getLandBlock(x >>> 3, y >>> 3)

    tiles(((y & 0x7) << 3) + (x & 0x7))
  }

  def getLandBlock(x: Int, y: Int): Array[LandTile] = {
    if (x < 0 || y < 0 || x >= blockWidth || y >= blockHeight) {
      TileMatrix.invalidLandBlock
    } else {
      if (landTiles(x) == null) {
        landTiles(x) = new Array(blockHeight)
      }

      if (landTiles(x)(y) == null) {
        landTiles(x)(y) = readLandBlock(x, y)
      }

      landTiles(x)(y)
    }
  }

  def readLandBlock(x: Int, y: Int): Array[LandTile] = {
    val rawOffset: Int = ((x * blockHeight) + y) * 196 + 4
    val uopOffsetOpt: Option[Int] = uopIndexOpt.map(_.lookup(rawOffset))
    val offset = uopOffsetOpt.getOrElse(rawOffset)

    mapFile.seek(offset)

    val tiles: Array[LandTile] = new Array(64)

    tiles.indices.foreach { idx =>
      val id: Int = mapFile.readShortLE()
      val z: Int = mapFile.readByte()
      tiles(idx) = LandTile(id, z)
    }

    tiles
  }

  def getStaticTiles(x: Int, y: Int): Seq[StaticTile] = {
    val block: Array[Array[mutable.Buffer[StaticTile]]] = getStaticBlock(x >>> 3, y >>> 3)

    block(x & 0x7)(y & 0x7)
  }

  def getStaticBlock(x: Int, y: Int): Array[Array[mutable.Buffer[StaticTile]]] = {
    if (x < 0 || y < 0 || x >= blockWidth || y >= blockHeight) {
      TileMatrix.emptyStaticBlock
    } else {
      if (staticTiles(x) == null) {
        staticTiles(x) = new Array(blockHeight)
      }

      if (staticTiles(x)(y) == null) {
        staticTiles(x)(y) = readStaticBlock(x, y)
      }

      staticTiles(x)(y)
    }
  }

  def readStaticBlock(x: Int, y: Int): Array[Array[mutable.Buffer[StaticTile]]] = {
    val offset: Int = ((x * blockHeight) + y) * 12
    staticIndexFile.seek(offset)
    val lookup: Int = staticIndexFile.readIntLE()
    val length: Int = staticIndexFile.readIntLE()

    if (lookup <= 0 || length <= 0) {
      TileMatrix.emptyStaticBlock
    } else {
      val count: Int = length / 7

      staticFile.seek(lookup)

      val tiles: Array[Array[mutable.Buffer[StaticTile]]] = Array.fill(8, 8)(mutable.ArrayBuffer.empty)

      Range(0, count).foreach { _ =>
        val id: Int = staticFile.readShortLE()
        val x: Int = staticFile.readUnsignedByte()
        val y: Int = staticFile.readUnsignedByte()
        val z: Int = staticFile.readByte()
        val hue: Int = staticFile.readShortLE()

        tiles(x & 0x7)(y & 0x7).append(StaticTile(id, z, hue))
      }

      tiles
    }
  }
}

object TileMatrix {
  val invalidLandBlock: Array[LandTile] = new Array(64)
  val emptyStaticBlock: Array[Array[mutable.Buffer[StaticTile]]] = Array.fill[mutable.Buffer[StaticTile]](8, 8)(null)
}
