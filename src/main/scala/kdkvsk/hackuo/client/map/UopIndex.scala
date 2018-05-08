package kdkvsk.hackuo.client.map

import java.io.RandomAccessFile
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.SortedSet
import scala.collection.{SortedMap, mutable}

case class UopIndexEntry(offset: Int, length: Int, order: Int, totalLength: Int)

case class UopIndex(path: Path, fileSize: Int, version: Int, entries: SortedMap[Int, UopIndexEntry]) {
  def lookup(offset: Int): Int = {
    val it = entries.valuesIteratorFrom(offset)
    if (it.hasNext) {
      val entry: UopIndexEntry = it.next()
      entry.offset + (offset + entry.length - entry.totalLength)
    } else {
      fileSize
    }
  }
}

object UopIndex extends LazyLogging {
  import kdkvsk.hackuo.lib.DataInputUtils.DataInputOps

  def load(path: Path): UopIndex = {
    logger.info(s"loading UOP index file from $path")

    if (!Files.exists(path)) {
      throw new IllegalArgumentException(s"UOP file $path does not exist")
    }

    val fileSize: Long = Files.size(path)

    val f: RandomAccessFile = new RandomAccessFile(path.toFile, "r")

    try {
      if (f.readIntLE() != 0x50594D) {
        throw new IllegalArgumentException(s"Invalid UOP file $path")
      }

      val entries: mutable.Buffer[UopIndexEntry] = mutable.ListBuffer.empty

      val version: Int = f.readIntLE()
      f.skipBytes(4)
      var nextTable: Int = f.readIntLE()

      do {
        f.seek(nextTable)
        val count: Int = f.readIntLE()
        nextTable = f.readIntLE()
        f.skipBytes(4)

        Range(0, count).foreach { _ =>
          val offset: Int = f.readIntLE()

          if (offset == 0) {
            f.skipBytes(30)
          } else {
            f.skipBytes(8)
            val length: Int = f.readIntLE()
            entries.append(UopIndexEntry(offset, length, 0, 0))
            f.skipBytes(18)
          }
        }

      } while (nextTable != 0 && nextTable < fileSize)

      val entriesArray: Array[UopIndexEntry] = entries.sortBy(_.offset).map { entry =>
        f.seek(entry.offset + 2)
        val dataOffset: Int = f.readShortLE()
        f.skipBytes(dataOffset)
        val order: Int = f.readIntLE()

        entry.copy(offset = entry.offset + dataOffset + 4, order = order)
      }.sortBy(_.order).toArray

      val (entriesMap: SortedMap[Int, UopIndexEntry], _: Int) = entriesArray.foldLeft((SortedMap.empty[Int, UopIndexEntry], 0)) {
        case ((m, cl), e) =>
          (m.updated(cl + e.length, e.copy(totalLength = cl + e.length)), cl + e.length)
      }

      UopIndex(path, fileSize.toInt, version, entriesMap)
    } finally {
      f.close()
    }
  }
}

