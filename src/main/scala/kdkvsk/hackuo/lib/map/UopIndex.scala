package kdkvsk.hackuo.lib.map

import java.io.RandomAccessFile
import java.nio.file.{Files, Path}

import scala.collection.mutable

case class UopIndexEntry(offset: Int, length: Int, order: Int)

case class UopIndex(path: Path, fileSize: Long, version: Int, entries: Array[UopIndexEntry])

object UopIndex {
  def load(path: Path): UopIndex = {
    if (!Files.exists(path)) {
      throw new IllegalArgumentException(s"UOP file $path does not exist")
    }

    val fileSize: Long = Files.size(path)

    val f: RandomAccessFile = new RandomAccessFile(path.toFile, "r")

    try {
      if (f.readInt() != 0x50594D) {
        throw new IllegalArgumentException(s"Invalid UOP file")
      }

      val entries: mutable.Buffer[UopIndexEntry] = mutable.ListBuffer.empty

      val version: Int = f.readInt()
      f.skipBytes(4)

      var nextTable: Int = f.readInt()

      do {
        f.seek(nextTable)
        val count: Int = f.readInt()
        nextTable = f.readInt()
        f.skipBytes(4)

        Range(0, count).foreach { _ =>
          val offset: Int = f.readInt()

          if (offset == 0) {
            f.skipBytes(30)
          } else {
            f.skipBytes(8)
            val length: Int = f.readInt()
            entries.append(UopIndexEntry(offset, length, 0))
            f.skipBytes(18)
          }
        }

      } while (nextTable != 0 && nextTable < fileSize)

      val entriesArray: Array[UopIndexEntry] = entries.sortBy(_.offset)
        .map { entry =>
          f.seek(entry.offset + 2)
          val dataOffset: Int = f.readShort() & 0xFFFF
          f.skipBytes(dataOffset)
          val order: Int = f.readInt()

          entry.copy(offset = dataOffset + 4, order = order)
        }
        .sortBy(_.order)
        .toArray

      UopIndex(path, fileSize, version, entriesArray)
    } finally {
      f.close()
    }
  }
}

